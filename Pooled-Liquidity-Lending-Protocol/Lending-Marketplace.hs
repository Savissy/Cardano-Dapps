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
  , pdBorrowers      :: [(PubKeyHash, Integer)] -- NEW
  , pdInterestRate   :: Integer -- percent
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

{-# INLINABLE lookupAmount #-}
lookupAmount :: PubKeyHash -> [(PubKeyHash, Integer)] -> Integer
lookupAmount _ [] = 0
lookupAmount p ((k,v):xs)
  | p == k    = v
  | otherwise = lookupAmount p xs

{-# INLINABLE upsert #-}
upsert :: PubKeyHash -> Integer -> [(PubKeyHash, Integer)] -> [(PubKeyHash, Integer)]
upsert p v [] = [(p,v)]
upsert p v ((k,x):xs)
  | p == k    = (k, v) : xs
  | otherwise = (k, x) : upsert p v xs

{-# INLINABLE remove #-}
remove :: PubKeyHash -> [(PubKeyHash, Integer)] -> [(PubKeyHash, Integer)]
remove _ [] = []
remove p ((k,v):xs)
  | p == k    = xs
  | otherwise = (k,v) : remove p xs

{-# INLINABLE getContinuingOutput #-}
getContinuingOutput :: ScriptContext -> TxOut
getContinuingOutput ctx =
  case getContinuingOutputs ctx of
    [o] -> o
    _   -> traceError "expected exactly one continuing output"

{-# INLINABLE getpDatum #-}
getpDatum :: TxOut -> PoolDatum
getpDatum o = case txOutDatum o of
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
    -- Deposit
    -------------------------------------------------
    Deposit ->
      traceIfFalse "not signed" signerPresent &&
      traceIfFalse "liquidity mismatch" liquidityCorrect &&
      traceIfFalse "shares mismatch" sharesCorrect

    -------------------------------------------------
    -- Withdraw
    -------------------------------------------------
    Withdraw ->
      traceIfFalse "not signed" signerPresent &&
      traceIfFalse "no shares" hasShares &&
      traceIfFalse "withdraw incorrect" withdrawCorrect

    -------------------------------------------------
    -- Borrow
    -------------------------------------------------
    Borrow amt ->
      traceIfFalse "not signed" signerPresent &&
      traceIfFalse "insufficient pool" (pdTotalLiquidity dat >= amt) &&
      traceIfFalse "borrow not recorded" borrowRecorded

    -------------------------------------------------
    -- Repay
    -------------------------------------------------
    Repay ->
      traceIfFalse "not signed" signerPresent &&
      traceIfFalse "no debt" (borrowedBySigner > 0) &&
      traceIfFalse "repayment insufficient" repaidEnough &&
      traceIfFalse "borrower not cleared" borrowerCleared

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
    newDatum = getpDatum ownOutput

    -------------------------------------------------
    -- Deposit logic
    -------------------------------------------------

    deposited :: Integer
    deposited = newValue - oldValue

    mintedShares :: Integer
    mintedShares =
      if pdTotalShares dat == 0
        then deposited
        else (deposited * pdTotalShares dat) `divide` oldValue

    liquidityCorrect :: Bool
    liquidityCorrect =
      pdTotalLiquidity newDatum ==
        pdTotalLiquidity dat + deposited

    sharesCorrect :: Bool
    sharesCorrect =
      pdTotalShares newDatum ==
        pdTotalShares dat + mintedShares

    -------------------------------------------------
    -- Withdraw logic
    -------------------------------------------------

    hasShares :: Bool
    hasShares = lookupAmount signer (pdLenders dat) > 0

    burnedShares :: Integer
    burnedShares = pdTotalShares dat - pdTotalShares newDatum

    withdrawn :: Integer
    withdrawn = oldValue - newValue

    expectedWithdraw :: Integer
    expectedWithdraw =
      (burnedShares * oldValue) `divide` pdTotalShares dat

    withdrawCorrect :: Bool
    withdrawCorrect = withdrawn == expectedWithdraw

    -------------------------------------------------
    -- Borrow logic
    -------------------------------------------------

    previousBorrow :: Integer
    previousBorrow = lookupAmount signer (pdBorrowers dat)

    borrowRecorded :: Bool
    borrowRecorded =
      lookupAmount signer (pdBorrowers newDatum)
        == previousBorrow + case action of Borrow a -> a; _ -> 0

    -------------------------------------------------
    -- Repay logic
    -------------------------------------------------

    borrowedBySigner :: Integer
    borrowedBySigner = lookupAmount signer (pdBorrowers dat)

    interest :: Integer
    interest = (borrowedBySigner * pdInterestRate dat) `divide` 100

    totalOwed :: Integer
    totalOwed = borrowedBySigner + interest

    repaidEnough :: Bool
    repaidEnough = newValue >= oldValue + totalOwed

    borrowerCleared :: Bool
    borrowerCleared =
      lookupAmount signer (pdBorrowers newDatum) == 0

-------------------------------------------------
-- Untyped wrapper
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
-- Address + CBOR
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

-------------------------------------------------
-- Main
-------------------------------------------------

main :: IO ()
main = do
  let network = C.Testnet (C.NetworkMagic 1)

  writeValidator "lending_pool.plutus" validator
  writeCBOR "lending_pool.cbor" validator

  putStrLn "\n--- Borrower-Tracked ADA Lending Pool ---"
  putStrLn $ "Address: " <> toBech32ScriptAddress network validator
  putStrLn "---------------------------------------"

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
  LBS.writeFile path (Serialise.serialise val)
  putStrLn $ "Validator written to: " <> path

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
  let bytes = LBS.toStrict (Serialise.serialise val)
  BS.writeFile path (B16.encode bytes)
  putStrLn $ "CBOR hex written to: " <> path
