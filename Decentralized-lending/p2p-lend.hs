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

--------------------------------------------------------------------------------
-- Datum & Redeemer
--------------------------------------------------------------------------------

data LoanDatum = LoanDatum
    { ldBorrower   :: PubKeyHash
    , ldLender     :: PubKeyHash
    , ldCollateral :: Integer
    , ldPrincipal  :: Integer
    , ldInterest   :: Integer
    , ldLTV        :: Integer
    }
PlutusTx.unstableMakeIsData ''LoanDatum

data LoanAction = OpenLoan | RepayLoan
PlutusTx.unstableMakeIsData ''LoanAction

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx =
    txSignedBy (scriptContextTxInfo ctx) pkh

--------------------------------------------------------------------------------
-- Validator Logic
--------------------------------------------------------------------------------

{-# INLINABLE mkLoanValidator #-}
mkLoanValidator :: LoanDatum -> LoanAction -> ScriptContext -> Bool
mkLoanValidator dat action ctx =
    case action of

        OpenLoan ->
            traceIfFalse "borrower not signed" borrowerSigned

        RepayLoan ->
            traceIfFalse "borrower not signed" borrowerSigned &&
            traceIfFalse "lender not paid" lenderPaid &&
            traceIfFalse "collateral not returned" collateralReturned
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    borrowerSigned :: Bool
    borrowerSigned = signedBy (ldBorrower dat) ctx

    lenderPaid :: Bool
    lenderPaid =
        let paid =
                valueOf (valuePaidTo info (ldLender dat)) adaSymbol adaToken
        in paid >= (ldPrincipal dat + ldInterest dat)

    collateralReturned :: Bool
    collateralReturned =
        case findOwnInput ctx of
            Nothing -> traceError "missing script input"
            Just i  ->
                let inVal = txOutValue (txInInfoResolved i)
                in valueOf inVal adaSymbol adaToken == ldCollateral dat

--------------------------------------------------------------------------------
-- Untyped Wrapper
--------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkLoanValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator =
    mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

--------------------------------------------------------------------------------
-- Script Address & CBOR Helpers
--------------------------------------------------------------------------------

plutusValidatorHash :: Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes = Serialise.serialise val
    in PlutusV2.ValidatorHash . toBuiltin . SBS.fromShort . SBS.toShort $ LBS.toStrict bytes

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

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

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
    let bytes = LBS.toStrict (Serialise.serialise val)
        hex   = B16.encode bytes
    BS.writeFile path hex
    putStrLn $ "CBOR hex written to: " <> path

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    writeCBOR "loan_validator.cbor" validator
    putStrLn "\n--- Flexible Loan Validator Compiled ---"
    putStrLn $ "Script Address: " <> P.show plutusScriptAddress
