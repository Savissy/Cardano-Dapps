{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>), take)
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum & Redeemer
------------------------------------------------------------------------

data EscrowDatum = EscrowDatum
    { edBuyer     :: PubKeyHash
    , edSeller    :: PubKeyHash
    , edAmount    :: Integer
    , edDelivered :: Bool
    }
PlutusTx.unstableMakeIsData ''EscrowDatum

data EscrowAction
    = LockFunds
    | ConfirmDelivery
    | ClaimFunds
    | RefundBuyer
PlutusTx.unstableMakeIsData ''EscrowAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx =
    txSignedBy (scriptContextTxInfo ctx) pkh

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkEscrowValidator #-}
mkEscrowValidator :: EscrowDatum -> EscrowAction -> ScriptContext -> Bool
mkEscrowValidator dat action ctx =
    case action of

        ------------------------------------------------------------
        -- Buyer locks funds
        ------------------------------------------------------------
        LockFunds ->
            traceIfFalse "buyer not signed" buyerSigned

        ------------------------------------------------------------
        -- Buyer confirms delivery (state update)
        ------------------------------------------------------------
        ConfirmDelivery ->
            traceIfFalse "buyer not signed" buyerSigned &&
            traceIfFalse "already delivered" (not $ edDelivered dat)

        ------------------------------------------------------------
        -- Seller claims funds after confirmation
        ------------------------------------------------------------
        ClaimFunds ->
            traceIfFalse "seller not signed" sellerSigned &&
            traceIfFalse "delivery not confirmed" (edDelivered dat) &&
            traceIfFalse "seller not paid" sellerPaid

        ------------------------------------------------------------
        -- Buyer refunds BEFORE confirmation
        ------------------------------------------------------------
        RefundBuyer ->
            traceIfFalse "buyer not signed" buyerSigned &&
            traceIfFalse "delivery already confirmed" (not $ edDelivered dat) &&
            traceIfFalse "buyer not refunded" buyerPaid

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    buyerSigned :: Bool
    buyerSigned =
        signedBy (edBuyer dat) ctx

    sellerSigned :: Bool
    sellerSigned =
        signedBy (edSeller dat) ctx

    sellerPaid :: Bool
    sellerPaid =
        valueOf
            (valuePaidTo info (edSeller dat))
            adaSymbol
            adaToken >= edAmount dat

    buyerPaid :: Bool
    buyerPaid =
        valueOf
            (valuePaidTo info (edBuyer dat))
            adaSymbol
            adaToken >= edAmount dat

------------------------------------------------------------------------
-- Untyped Wrapper
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkEscrowValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator =
    mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-------------------------------------------------
-- SCRIPT ADDRESS (OFF-CHAIN HELPERS)
-------------------------------------------------

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
    in P.show shelleyAddr

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

    writeValidator "escrow.plutus" validator
    writeCBOR      "escrow.cbor"   validator

    putStrLn "Escrow validator generated"
