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
signedBy pkh ctx =
    txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE getLenderShares #-}
getLenderShares :: PubKeyHash -> [(PubKeyHash, Integer)] -> Integer
getLenderShares _ [] = 0
getLenderShares p ((k,v):xs)
    | p == k    = v
    | otherwise = getLenderShares p xs

{-# INLINABLE adaLockedAtOwnScript #-}
adaLockedAtOwnScript :: ScriptContext -> Integer
adaLockedAtOwnScript ctx =
    let
        info = scriptContextTxInfo ctx
        vh   = ownHash ctx
        outs = txInfoOutputs info
    in
        foldr
            (\o acc ->
                case txOutAddress o of
                    Address (ScriptCredential vh') _ | vh' == vh ->
                        acc + valueOf (txOutValue o) adaSymbol adaToken
                    _ ->
                        acc
            )
            0
            outs


-------------------------------------------------
-- Validator
-------------------------------------------------

{-# INLINABLE mkPoolValidator #-}
mkPoolValidator :: PoolDatum -> PoolAction -> ScriptContext -> Bool
mkPoolValidator dat action ctx =
    case action of

        -------------------------------------------------
        -- Lender deposits ADA, receives shares
        -------------------------------------------------
        Deposit ->
            traceIfFalse "not signed" signerPresent &&
            traceIfFalse "no ADA deposited" adaDeposited

        -------------------------------------------------
        -- Lender withdraws proportional ADA
        -------------------------------------------------
        Withdraw ->
            traceIfFalse "not signed" signerPresent &&
            traceIfFalse "no shares" hasShares &&
            traceIfFalse "insufficient pool" poolHasLiquidity

        -------------------------------------------------
        -- Borrower borrows from pool
        -------------------------------------------------
        Borrow amt ->
            traceIfFalse "pool empty" (pdTotalLiquidity dat >= amt)

        -------------------------------------------------
        -- Borrower repays principal + interest
        -------------------------------------------------
        Repay ->
            traceIfFalse "repayment insufficient" repaidEnough
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signer :: PubKeyHash
    signer =
        case txInfoSignatories info of
            []    -> traceError "no signer"
            s:_   -> s

    signerPresent :: Bool
    signerPresent = signedBy signer ctx

    adaDeposited :: Bool
    adaDeposited =
        adaLockedAtOwnScript ctx > pdTotalLiquidity dat

    hasShares :: Bool
    hasShares =
        getLenderShares signer (pdLenders dat) > 0

    poolHasLiquidity :: Bool
    poolHasLiquidity =
        pdTotalLiquidity dat > 0

    repaidEnough :: Bool
    repaidEnough =
        let interest =
                (pdBorrowed dat * pdInterestRate dat) `divide` 100
        in adaLockedAtOwnScript ctx >= pdBorrowed dat + interest

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
validator =
    mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-------------------------------------------------
-- Address Helpers (Same as Your Project)
-------------------------------------------------

plutusValidatorHash :: Validator -> ValidatorHash
plutusValidatorHash v =
    let bytes = Serialise.serialise v
        short = SBS.toShort (LBS.toStrict bytes)
    in ValidatorHash (toBuiltin (SBS.fromShort short))

plutusScriptAddress :: Address
plutusScriptAddress =
    Address
        (ScriptCredential (plutusValidatorHash validator))
        Nothing

-------------------------------------------------
-- Bech32 Address
-------------------------------------------------

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let
        serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash   =
            C.hashScript
                (C.PlutusScript C.PlutusScriptV2 plutusScript)

        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in
        T.unpack (C.serialiseAddress shelleyAddr)


-------------------------------------------------
-- CBOR HEX
-------------------------------------------------

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
-- File Writers
-------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
    let bytes = LBS.toStrict (Serialise.serialise val)
        hex   = B16.encode bytes
    BS.writeFile path hex
    putStrLn $ "CBOR hex written to: " <> path

-------------------------------------------------
-- Main
-------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "lending_pool.plutus" validator
    writeCBOR      "lending_pool.cbor"   validator

    let vh      = plutusValidatorHash validator
        addr    = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator
        cborHex = validatorToCBORHex validator

    putStrLn "\n--- Fixed-Rate ADA Lending Pool (Multi-Lender) ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Script Address: " <> P.show addr
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn $ "CBOR Hex (first 120 chars): " <> P.take 120 cborHex <> "..."
    putStrLn "--------------------------------------------------"
