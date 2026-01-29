{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2

import Plutus.V1.Ledger.Value
    ( AssetClass
    , assetClassValueOf
    , valueOf
    , adaSymbol
    , adaToken
    )

import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------
-- Datum & Redeemer
------------------------------------------------------------

data Investor = Investor
    { invPkh    :: PubKeyHash
    , invAmount :: Integer
    }
PlutusTx.unstableMakeIsData ''Investor

data InvoiceDatum = InvoiceDatum
    { idIssuer     :: PubKeyHash
    , idInvoiceNFT :: AssetClass
    , idFaceValue  :: Integer
    , idRepayment  :: Integer
    , idInvestors  :: [Investor]   -- exactly 0 or 1
    , isRepaid     :: Bool         -- ✅ NEW
    }
PlutusTx.unstableMakeIsData ''InvoiceDatum

data InvoiceAction
    = FundInvoice
    | RepayInvoice
PlutusTx.unstableMakeIsData ''InvoiceAction

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

{-# INLINABLE paidAda #-}
paidAda :: TxInfo -> PubKeyHash -> Integer
paidAda info pkh =
    valueOf (valuePaidTo info pkh) adaSymbol adaToken

{-# INLINABLE notFunded #-}
notFunded :: InvoiceDatum -> Bool
notFunded dat =
    case idInvestors dat of
        [] -> True
        _  -> False

------------------------------------------------------------
-- NFT Preservation (funding only)
------------------------------------------------------------

{-# INLINABLE nftPreserved #-}
nftPreserved :: InvoiceDatum -> ScriptContext -> Bool
nftPreserved dat ctx =
    case getContinuingOutputs ctx of
        [o] ->
            assetClassValueOf (txOutValue o) (idInvoiceNFT dat) == 1
        _ -> False

------------------------------------------------------------
-- Validator
------------------------------------------------------------

{-# INLINABLE mkInvoiceValidator #-}
mkInvoiceValidator :: InvoiceDatum -> InvoiceAction -> ScriptContext -> Bool
mkInvoiceValidator dat action ctx =
    case action of

        ----------------------------------------------------
        -- Investor funds invoice
        ----------------------------------------------------
        FundInvoice ->
            traceIfFalse "already funded"        (notFunded dat) &&
            traceIfFalse "already repaid"        (not $ isRepaid dat) &&
            traceIfFalse "issuer not paid"       issuerPaid &&
            traceIfFalse "NFT missing"           (nftPreserved dat ctx) &&
            traceIfFalse "investor must sign"    investorSigned

        ----------------------------------------------------
        -- Issuer repays investor
        ----------------------------------------------------
        RepayInvoice ->
            traceIfFalse "already repaid"        (not $ isRepaid dat) &&
            traceIfFalse "no investor"           hasInvestor &&
            traceIfFalse "repayment insufficient" repaymentEnough &&
            traceIfFalse "investor not paid"     investorPaid

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ----------------------------------------------------
    -- Funding checks
    ----------------------------------------------------

    issuerPaid :: Bool
    issuerPaid =
        paidAda info (idIssuer dat) >= idFaceValue dat

    investorSigned :: Bool
    investorSigned =
        case txInfoSignatories info of
            [_] -> True
            _   -> False

    ----------------------------------------------------
    -- Repayment checks
    ----------------------------------------------------

    hasInvestor :: Bool
    hasInvestor =
        case idInvestors dat of
            [_] -> True
            _   -> False

    repaymentEnough :: Bool
    repaymentEnough =
        valueOf
            (mconcat (map txOutValue (txInfoOutputs info)))
            adaSymbol
            adaToken
            >= idRepayment dat

    investorPaid :: Bool
    investorPaid =
        case idInvestors dat of
            [inv] ->
                let profit = idRepayment dat - idFaceValue dat
                in paidAda info (invPkh inv)
                    >= invAmount inv + profit
            _ -> False

------------------------------------------------------------
-- Untyped Wrapper
------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkInvoiceValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator =
    mkValidatorScript
        $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------
-- Script Address
------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes = Serialise.serialise val
    in PlutusV2.ValidatorHash
        (toBuiltin (SBS.fromShort (SBS.toShort (LBS.toStrict bytes))))

plutusScriptAddress :: Address
plutusScriptAddress =
    Address
        (ScriptCredential (plutusValidatorHash validator))
        Nothing

------------------------------------------------------------
-- Bech32 Address
------------------------------------------------------------

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash   = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

------------------------------------------------------------
-- Write CBOR
------------------------------------------------------------

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
    BS.writeFile path . B16.encode . LBS.toStrict $ Serialise.serialise val
    putStrLn $ "CBOR written to: " <> path

------------------------------------------------------------
-- Main
------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
    writeCBOR "invoice_financing.cbor" validator
    putStrLn "Invoice Financing Validator Ready"
    putStrLn $ "Address: " <> toBech32ScriptAddress network validator
