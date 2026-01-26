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
    , idInvestors  :: [Investor]
    }
PlutusTx.unstableMakeIsData ''InvoiceDatum

data InvoiceAction
    = FundInvoice
    | RepayInvoice
PlutusTx.unstableMakeIsData ''InvoiceAction

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx =
    txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE paidAda #-}
paidAda :: TxInfo -> PubKeyHash -> Integer
paidAda info pkh =
    valueOf (valuePaidTo info pkh) adaSymbol adaToken

------------------------------------------------------------
-- Validator
------------------------------------------------------------

{-# INLINABLE mkInvoiceValidator #-}
mkInvoiceValidator :: InvoiceDatum -> InvoiceAction -> ScriptContext -> Bool
mkInvoiceValidator dat action ctx =
    case action of

        ----------------------------------------------------
        -- Investors fund (issuer controls datum evolution)
        ----------------------------------------------------
        FundInvoice ->
            traceIfFalse "issuer signature missing" issuerSigned

        ----------------------------------------------------
        -- Client repays and profit is split
        ----------------------------------------------------
        RepayInvoice ->
            traceIfFalse "repayment insufficient" repaymentEnough &&
            traceIfFalse "investors not paid" investorsPaid &&
            traceIfFalse "invoice NFT not released" nftReleased

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    issuerSigned :: Bool
    issuerSigned =
        signedBy (idIssuer dat) ctx

    ----------------------------------------------------
    -- Total repayment must be present
    ----------------------------------------------------
    repaymentEnough :: Bool
    repaymentEnough =
        let totalOut =
                valueOf
                    (mconcat (map txOutValue (txInfoOutputs info)))
                    adaSymbol
                    adaToken
        in totalOut >= idRepayment dat

    ----------------------------------------------------
    -- Each investor gets principal + proportional profit
    ----------------------------------------------------
    investorsPaid :: Bool
    investorsPaid =
        all paidInvestor (idInvestors dat)

    paidInvestor :: Investor -> Bool
    paidInvestor inv =
        let totalProfit =
                idRepayment dat - idFaceValue dat

            investorProfit =
                (invAmount inv * totalProfit)
                    `divide` idFaceValue dat
        in paidAda info (invPkh inv)
            >= invAmount inv + investorProfit

    ----------------------------------------------------
    -- NFT must leave the script
    ----------------------------------------------------
    nftReleased :: Bool
    nftReleased =
        case findOwnInput ctx of
            Nothing -> traceError "no script input"
            Just i  ->
                let v = txOutValue (txInInfoResolved i)
                in assetClassValueOf v (idInvoiceNFT dat) == 1

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
-- Validator Hash & Address
------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes  = Serialise.serialise val
        short  = SBS.toShort (LBS.toStrict bytes)
    in PlutusV2.ValidatorHash (toBuiltin (SBS.fromShort short))

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

------------------------------------------------------------
-- CBOR Helpers
------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
    let bytes = LBS.toStrict (Serialise.serialise val)
    BS.writeFile path (B16.encode bytes)
    putStrLn $ "CBOR hex written to: " <> path

------------------------------------------------------------
-- Main
------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "invoice_financing.plutus" validator
    writeCBOR      "invoice_financing.cbor"   validator

    putStrLn "\n--- Invoice Financing Contract ---"
    putStrLn $ "Validator Hash: " <> P.show (plutusValidatorHash validator)
    putStrLn $ "Script Address: " <> P.show plutusScriptAddress
    putStrLn $ "Bech32 Address: " <> toBech32ScriptAddress network validator
    putStrLn "----------------------------------"
