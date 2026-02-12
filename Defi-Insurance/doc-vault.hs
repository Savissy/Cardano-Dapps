{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, FilePath, String, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import Plutus.V1.Ledger.Value
  ( AssetClass(..)
  , assetClassValueOf
  )

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-------------------------------------------------
-- DATUM
-------------------------------------------------

data DocVaultDatum = DocVaultDatum
  { dvClaimer  :: PubKeyHash
  , dvDoc      :: AssetClass
  , dvExecuted :: Bool
  }
PlutusTx.unstableMakeIsData ''DocVaultDatum

-------------------------------------------------
-- REDEEMER
-------------------------------------------------

data DocVaultAction
  = MarkExecuted      -- admin/signers set dvExecuted = True, MUST keep NFT locked + same claimer/doc
  | RemoveToClaimer   -- admin/signers can unlock ONLY if executed=True and NFT is paid to claimer
PlutusTx.unstableMakeIsData ''DocVaultAction

-------------------------------------------------
-- CONFIG (EDIT THESE)
-------------------------------------------------

-- Put the *payment key hashes* of your pool signers here (hex bytes).
-- These are the same "pdSigners" you already use in your pool datum.
{-# INLINABLE poolSigners #-}
poolSigners :: [PubKeyHash]
poolSigners =
  [ PubKeyHash "33414de0df0b747686f8035ee6b8302a87b36b2770f4284c7eef4b26"
  , PubKeyHash "77d54a7afbf47a8f79611636c6de31c154b0420388dcb5c45772211a"
  ]

-------------------------------------------------
-- HELPERS
-------------------------------------------------

{-# INLINABLE isPoolSigner #-}
isPoolSigner :: TxInfo -> Bool
isPoolSigner info =
  let sigs = txInfoSignatories info
  in any (\pkh -> elem pkh poolSigners) sigs

{-# INLINABLE ownInput #-}
ownInput :: ScriptContext -> TxOut
ownInput ctx =
  case findOwnInput ctx of
    Nothing -> traceError "docvault: missing own input"
    Just i  -> txInInfoResolved i

{-# INLINABLE onlyOneContinuing #-}
onlyOneContinuing :: [TxOut] -> TxOut
onlyOneContinuing outs =
  case outs of
    [o] -> o
    _   -> traceError "docvault: expected exactly one continuing output"

{-# INLINABLE hasOneDoc #-}
hasOneDoc :: Value -> AssetClass -> Bool
hasOneDoc v ac = assetClassValueOf v ac == 1

-------------------------------------------------
-- VALIDATOR
-------------------------------------------------

{-# INLINABLE mkDocVault #-}
mkDocVault :: DocVaultDatum -> DocVaultAction -> ScriptContext -> Bool
mkDocVault dat action ctx =
  case action of

    -- Consume the doc-vault UTxO and re-create it at the SAME script address
    -- with dvExecuted=True, while preserving dvClaimer + dvDoc + the NFT.
    MarkExecuted ->
      traceIfFalse "docvault: pool signer required" (isPoolSigner info) &&
      traceIfFalse "docvault: must keep NFT in input" inputHasDoc &&
      traceIfFalse "docvault: must keep NFT locked" outputHasDoc &&
      traceIfFalse "docvault: datum must preserve claimer" sameClaimer &&
      traceIfFalse "docvault: datum must preserve doc assetclass" sameDoc &&
      traceIfFalse "docvault: must set executed=True" setsExecutedTrue

    -- Consume the UTxO and pay the doc NFT to the claimer address.
    -- Only allowed if dvExecuted=True and a pool signer signs.
    RemoveToClaimer ->
      traceIfFalse "docvault: pool signer required" (isPoolSigner info) &&
      traceIfFalse "docvault: not executed yet" (dvExecuted dat) &&
      traceIfFalse "docvault: must keep NFT in input" inputHasDoc &&
      traceIfFalse "docvault: NFT not paid to claimer" paidDocToClaimer

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputV :: Value
    inputV = txOutValue (ownInput ctx)

    inputHasDoc :: Bool
    inputHasDoc = hasOneDoc inputV (dvDoc dat)

    continuing :: [TxOut]
    continuing = getContinuingOutputs ctx

    out :: TxOut
    out = onlyOneContinuing continuing

    outputHasDoc :: Bool
    outputHasDoc = hasOneDoc (txOutValue out) (dvDoc dat)

    outDatum :: DocVaultDatum
    outDatum =
      case txOutDatum out of
        OutputDatum (Datum d) -> unsafeFromBuiltinData d
        OutputDatumHash _     -> traceError "docvault: expected inline datum"
        NoOutputDatum         -> traceError "docvault: missing output datum"

    sameClaimer :: Bool
    sameClaimer = dvClaimer outDatum == dvClaimer dat

    sameDoc :: Bool
    sameDoc = dvDoc outDatum == dvDoc dat

    setsExecutedTrue :: Bool
    setsExecutedTrue = dvExecuted outDatum == True

    paidDocToClaimer :: Bool
    paidDocToClaimer =
      -- Must pay the NFT (qty>=1) to dvClaimer
      assetClassValueOf (valuePaidTo info (dvClaimer dat)) (dvDoc dat) >= 1

-------------------------------------------------
-- UNTYPED WRAPPER
-------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
  if mkDocVault
      (unsafeFromBuiltinData d)
      (unsafeFromBuiltinData r)
      (unsafeFromBuiltinData c)
  then ()
  else error ()

validator :: Validator
validator =
  mkValidatorScript
    $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-------------------------------------------------
-- ADDRESS / SERIALISATION
-------------------------------------------------

plutusValidatorHash :: Validator -> ValidatorHash
plutusValidatorHash val =
  let bytes  = Serialise.serialise val
      short  = SBS.toShort (LBS.toStrict bytes)
  in ValidatorHash (toBuiltin (SBS.fromShort short))

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
  let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
      plutusScript :: C.PlutusScript C.PlutusScriptV2
      plutusScript = CS.PlutusScriptSerialised serialised
      scriptHash   = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
      shelleyAddr :: C.AddressInEra C.BabbageEra
      shelleyAddr =
        C.makeShelleyAddressInEra
          network
          (C.PaymentCredentialByScript scriptHash)
          C.NoStakeAddress
  in T.unpack (C.serialiseAddress shelleyAddr)

-------------------------------------------------
-- FILE OUTPUT
-------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
  LBS.writeFile path (Serialise.serialise val)
  putStrLn $ "Validator written to: " <> path

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
  let bytes = LBS.toStrict (Serialise.serialise val)
  BS.writeFile path (B16.encode bytes)
  putStrLn $ "CBOR hex written to: " <> path

-------------------------------------------------
-- MAIN
-------------------------------------------------

main :: IO ()
main = do
  let network = C.Testnet (C.NetworkMagic 1)

  writeValidator "docvault.plutus" validator
  writeCBOR      "docvault.cbor"   validator

  putStrLn "\n--- DocVault Validator ---"
  putStrLn $ "Validator Hash: " <> P.show (plutusValidatorHash validator)
  putStrLn $ "Bech32 Address: " <> toBech32ScriptAddress network validator
  putStrLn "--------------------------"
