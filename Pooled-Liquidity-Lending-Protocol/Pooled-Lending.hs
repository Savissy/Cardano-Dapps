{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, FilePath, putStrLn, String, (<>))
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)

import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-------------------------------------------------
-- Datum
-------------------------------------------------

data PoolDatum = PoolDatum
  { pdTotalLiquidity :: Integer
  , pdTotalShares    :: Integer
  , pdLenders        :: [(PubKeyHash, Integer)]
  , pdBorrowed       :: Integer
  , pdInterestRate   :: Integer -- e.g. 5 = 5%
  }

PlutusTx.unstableMakeIsData ''PoolDatum

-------------------------------------------------
-- Redeemer
-------------------------------------------------

data PoolAction
  = Deposit
  | Withdraw
  | Borrow Integer
  | Repay

PlutusTx.unstableMakeIsData ''PoolAction

-------------------------------------------------
-- Helpers
-------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE getLenderShares #-}
getLenderShares :: PubKeyHash -> [(PubKeyHash, Integer)] -> Integer
getLenderShares _ [] = 0
getLenderShares p ((k,v):xs)
  | p == k    = v
  | otherwise = getLenderShares p xs

{-# INLINABLE getContinuingOutput #-}
getContinuingOutput :: ScriptContext -> TxOut
getContinuingOutput ctx =
  case getContinuingOutputs ctx of
    [o] -> o
    _   -> traceError "expected exactly one continuing output"

{-# INLINABLE getDatum #-}
getDatum :: TxOut -> PoolDatum
getDatum o = case txOutDatum o of
  OutputDatum (Datum d) -> unsafeFromBuiltinData d
  _ -> traceError "datum missing"

-------------------------------------------------
-- Validator
-------------------------------------------------

{-# INLINABLE mkPoolValidator #-}
mkPoolValidator :: PoolDatum -> PoolAction -> ScriptContext -> Bool
mkPoolValidator dat action ctx =
  case action of

    -------------------------------------------------
    -- Deposit (mint shares proportionally)
    -------------------------------------------------
    Deposit ->
      traceIfFalse "not signed" signerPresent &&
      traceIfFalse "invalid share mint" sharesCorrect &&
      traceIfFalse "liquidity mismatch" liquidityCorrect

    -------------------------------------------------
    -- Withdraw (burn shares proportionally)
    -------------------------------------------------
    Withdraw ->
      traceIfFalse "not signed" signerPresent &&
      traceIfFalse "no shares" hasShares &&
      traceIfFalse "withdraw not proportional" withdrawCorrect

    -------------------------------------------------
    -- Borrow
    -------------------------------------------------
    Borrow amt ->
      traceIfFalse "insufficient pool" (pdTotalLiquidity dat >= amt)

    -------------------------------------------------
    -- Repay (principal + interest increases share price)
    -------------------------------------------------
    Repay ->
      traceIfFalse "repayment insufficient" repaidEnough

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signer :: PubKeyHash
    signer = case txInfoSignatories info of
      []    -> traceError "no signer"
      s : _ -> s

    signerPresent :: Bool
    signerPresent = signedBy signer ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "input missing"
      Just i  -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = getContinuingOutput ctx

    oldValue :: Integer
    oldValue = valueOf (txOutValue ownInput) adaSymbol adaToken

    newValue :: Integer
    newValue = valueOf (txOutValue ownOutput) adaSymbol adaToken

    newDatum :: PoolDatum
    newDatum = getDatum ownOutput

    deposited :: Integer
    deposited = newValue - oldValue

    mintedShares :: Integer
    mintedShares =
      if pdTotalShares dat == 0
        then deposited
        else (deposited * pdTotalShares dat) `divide` oldValue

    sharesCorrect :: Bool
    sharesCorrect =
      pdTotalShares newDatum ==
        pdTotalShares dat + mintedShares

    liquidityCorrect :: Bool
    liquidityCorrect =
      pdTotalLiquidity newDatum ==
        pdTotalLiquidity dat + deposited

    hasShares :: Bool
    hasShares =
      getLenderShares signer (pdLenders dat) > 0

    burnedShares :: Integer
    burnedShares =
      pdTotalShares dat - pdTotalShares newDatum

    withdrawn :: Integer
    withdrawn = oldValue - newValue

    expectedWithdraw :: Integer
    expectedWithdraw =
      (burnedShares * oldValue) `divide` pdTotalShares dat

    withdrawCorrect :: Bool
    withdrawCorrect =
      withdrawn == expectedWithdraw

    repaidEnough :: Bool
    repaidEnough =
      newValue >= oldValue + (pdBorrowed dat * pdInterestRate dat) `divide` 100

-------------------------------------------------
-- Untyped Wrapper
-------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
  if mkPoolValidator
      (unsafeFromBuiltinData d)
      (unsafeFromBuiltinData r)
      (unsafeFromBuiltinData c)
  then ()
  else error ()

validator :: Validator
validator = mkValidatorScript
  $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-------------------------------------------------
-- Address + CBOR (UNCHANGED)
-------------------------------------------------

plutusValidatorHash :: Validator -> ValidatorHash
plutusValidatorHash v =
  let bytes = Serialise.serialise v
      short = SBS.toShort (LBS.toStrict bytes)
  in ValidatorHash (toBuiltin (SBS.fromShort short))

plutusScriptAddress :: Address
plutusScriptAddress =
  Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
  let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
      plutusScript = CS.PlutusScriptSerialised serialised
      scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
      shelleyAddr :: C.AddressInEra C.BabbageEra
      shelleyAddr =
        C.makeShelleyAddressInEra
          network
          (C.PaymentCredentialByScript scriptHash)
          C.NoStakeAddress
  in T.unpack (C.serialiseAddress shelleyAddr)

validatorToCBORHex :: Validator -> String
validatorToCBORHex val =
  let bytes = LBS.toStrict $ Serialise.serialise val
  in BS.foldr (\b acc -> byteToHex b <> acc) "" bytes
  where
    hexChars = "0123456789abcdef"
    byteToHex b =
      let hi = P.fromIntegral b `P.div` 16
          lo = P.fromIntegral b `P.mod` 16
      in [ hexChars P.!! hi, hexChars P.!! lo ]

-------------------------------------------------
-- Main
-------------------------------------------------

main :: IO ()
main = do
  let network = C.Testnet (C.NetworkMagic 1)

  writeValidator "lending_pool.plutus" validator
  writeCBOR "lending_pool.cbor" validator

  putStrLn "\n--- Share-Based ADA Lending Pool ---"
  putStrLn $ "Bech32 Address: " <> toBech32ScriptAddress network validator
  putStrLn "----------------------------------"

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
  LBS.writeFile path (Serialise.serialise val)
  putStrLn $ "Validator written to: " <> path

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
  let bytes = LBS.toStrict (Serialise.serialise val)
  BS.writeFile path (B16.encode bytes)
  putStrLn $ "CBOR hex written to: " <> path
