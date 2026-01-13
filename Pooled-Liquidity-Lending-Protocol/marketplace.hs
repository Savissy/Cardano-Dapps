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
-- POOL DATUM (constant size)
-------------------------------------------------

data PoolDatum = PoolDatum
  { pdTotalLiquidity :: Integer
  , pdTotalShares    :: Integer
  , pdTotalBorrowed  :: Integer
  , pdInterestRate   :: Integer -- percent
  }

PlutusTx.unstableMakeIsData ''PoolDatum

-------------------------------------------------
-- BORROW POSITION DATUM
-------------------------------------------------

data BorrowDatum = BorrowDatum
  { bdBorrower  :: PubKeyHash
  , bdPrincipal :: Integer
  }

PlutusTx.unstableMakeIsData ''BorrowDatum

-------------------------------------------------
-- REDEEMER
-------------------------------------------------

data PoolAction
  = Deposit
  | Withdraw Integer      -- burned shares
  | Borrow Integer        -- amount
  | Repay Integer         -- principal
PlutusTx.unstableMakeIsData ''PoolAction

-------------------------------------------------
-- HELPERS
-------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx =
  txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE getContinuingOutput #-}
getContinuingOutput :: ScriptContext -> TxOut
getContinuingOutput ctx =
  case getContinuingOutputs ctx of
    [o] -> o
    _   -> traceError "expected exactly one continuing output"

{-# INLINABLE getpDatum #-}
getpDatum :: TxOut -> PoolDatum
getpDatum o =
  case txOutDatum o of
    OutputDatum (Datum d) -> unsafeFromBuiltinData d
    _ -> traceError "datum missing"

-------------------------------------------------
-- VALIDATOR
-------------------------------------------------

{-# INLINABLE mkPoolValidator #-}
mkPoolValidator :: PoolDatum -> PoolAction -> ScriptContext -> Bool
mkPoolValidator dat action ctx =
  case action of

    -------------------------------------------------
    -- DEPOSIT
    -------------------------------------------------
    Deposit ->
      traceIfFalse "not signed" signerPresent &&
      traceIfFalse "liquidity mismatch" liquidityCorrect &&
      traceIfFalse "shares mismatch" sharesCorrect

    -------------------------------------------------
    -- WITHDRAW
    -------------------------------------------------
    Withdraw burnedShares ->
      traceIfFalse "not signed" signerPresent &&
      traceIfFalse "invalid shares" (burnedShares > 0) &&
      traceIfFalse "withdraw incorrect" withdrawCorrect

    -------------------------------------------------
    -- BORROW
    -------------------------------------------------
    Borrow amt ->
      traceIfFalse "not signed" signerPresent &&
      traceIfFalse "insufficient liquidity"
        (pdTotalLiquidity dat >= amt) &&
      traceIfFalse "borrow accounting mismatch" borrowRecorded

    -------------------------------------------------
    -- REPAY
    -------------------------------------------------
    Repay principal ->
      traceIfFalse "not signed" signerPresent &&
      traceIfFalse "repay incorrect" repayCorrect

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signer :: PubKeyHash
    signer =
      case txInfoSignatories info of
        []    -> traceError "no signer"
        s : _ -> s

    signerPresent :: Bool
    signerPresent = signedBy signer ctx

    ownInput :: TxOut
    ownInput =
      case findOwnInput ctx of
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

    burnedShares :: Integer
    burnedShares =
        case action of
            Withdraw s -> s
            _          -> traceError "expected Withdraw"

    principal :: Integer
    principal =
        case action of
            Repay p -> p
            _       -> traceError "expected Repay"


    -------------------------------------------------
    -- DEPOSIT LOGIC
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
    -- WITHDRAW LOGIC
    -------------------------------------------------

    withdrawn :: Integer
    withdrawn = oldValue - newValue

    expectedWithdraw :: Integer
    expectedWithdraw =
      (burnedShares * oldValue) `divide` pdTotalShares dat

    withdrawCorrect :: Bool
    withdrawCorrect =
      withdrawn == expectedWithdraw &&
      pdTotalShares newDatum ==
        pdTotalShares dat - burnedShares

    -------------------------------------------------
    -- BORROW LOGIC
    -------------------------------------------------

    borrowRecorded :: Bool
    borrowRecorded =
      pdTotalBorrowed newDatum ==
        pdTotalBorrowed dat + case action of Borrow a -> a; _ -> 0

    -------------------------------------------------
    -- REPAY LOGIC
    -------------------------------------------------

    interest :: Integer
    interest =
      (principal * pdInterestRate dat) `divide` 100

    totalOwed :: Integer
    totalOwed = principal + interest

    repayCorrect :: Bool
    repayCorrect =
      newValue >= oldValue + totalOwed &&
      pdTotalBorrowed newDatum ==
        pdTotalBorrowed dat - principal

-------------------------------------------------
-- UNTYPED WRAPPER
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
-- ADDRESS + CBOR
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
-- MAIN
-------------------------------------------------

main :: IO ()
main = do
  let network = C.Testnet (C.NetworkMagic 1)

  writeValidator "lending_pool.plutus" validator
  writeCBOR "lending_pool.cbor" validator

  putStrLn "\n--- Single-UTxO ADA Lending Pool ---"
  putStrLn $ "Address: " <> toBech32ScriptAddress network validator
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
