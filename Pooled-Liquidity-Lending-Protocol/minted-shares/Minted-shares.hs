{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, FilePath, putStrLn, String)
import qualified Prelude as P

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Value
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import qualified PlutusTx.Builtins as Builtins
import qualified Plutus.V2.Ledger.Api as PlutusV2

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16

-------------------------------------------------
-- CONSTANTS
-------------------------------------------------

{-# INLINABLE shareTokenName #-}
shareTokenName :: TokenName
shareTokenName = TokenName "POOLSHARE"

-------------------------------------------------
-- POOL DATUM (GLOBAL STATE)
-------------------------------------------------

data PoolDatum = PoolDatum
  { pdTotalLiquidity :: Integer
  , pdTotalShares    :: Integer
  , pdTotalBorrowed  :: Integer
  , pdInterestRate   :: Integer
  }
PlutusTx.unstableMakeIsData ''PoolDatum

-------------------------------------------------
-- REDEEMER
-------------------------------------------------

data PoolAction
  = Deposit
  | Withdraw Integer
  | Borrow Integer
  | Repay Integer
PlutusTx.unstableMakeIsData ''PoolAction

-------------------------------------------------
-- HELPERS
-------------------------------------------------

{-# INLINABLE getContinuingOutput #-}
getContinuingOutput :: ScriptContext -> TxOut
getContinuingOutput ctx =
  case getContinuingOutputs ctx of
    [o] -> o
    _   -> traceError "expected exactly one continuing output"

{-# INLINABLE getPoolDatum #-}
getPoolDatum :: TxOut -> PoolDatum
getPoolDatum o =
  case txOutDatum o of
    OutputDatum (Datum d) -> unsafeFromBuiltinData d
    _ -> traceError "datum missing"

-------------------------------------------------
-- SHARE TOKEN MINTING POLICY
-------------------------------------------------

{-# INLINABLE mkSharePolicy #-}
mkSharePolicy :: ValidatorHash -> Integer -> ScriptContext -> Bool
mkSharePolicy poolVH shares ctx =
  traceIfFalse "pool utxo not spent" poolSpent &&
  traceIfFalse "wrong mint amount" mintCorrect
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    poolSpent :: Bool
    poolSpent =
      any (\i -> case txOutAddress (txInInfoResolved i) of
                   Address (ScriptCredential vh) _ -> vh == poolVH
                   _                               -> False
          ) (txInfoInputs info)

    mintCorrect :: Bool
    mintCorrect =
      assetClassValueOf (txInfoMint info)
        (AssetClass (ownCurrencySymbol ctx, shareTokenName))
        == shares

{-# INLINABLE mkSharePolicyUntyped #-}
mkSharePolicyUntyped :: ValidatorHash -> BuiltinData -> BuiltinData -> ()
mkSharePolicyUntyped vh r c =
  if mkSharePolicy vh
       (unsafeFromBuiltinData r)
       (unsafeFromBuiltinData c)
  then ()
  else error ()

sharePolicy :: ValidatorHash -> MintingPolicy
sharePolicy vh =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \h -> mkSharePolicyUntyped h ||])
      `PlutusTx.applyCode`
        PlutusTx.liftCode vh

-------------------------------------------------
-- POOL VALIDATOR
-------------------------------------------------

{-# INLINABLE mkPoolValidator #-}
mkPoolValidator :: PoolDatum -> PoolAction -> ScriptContext -> Bool
mkPoolValidator dat action ctx =
  case action of

    Deposit ->
      depositCorrect &&
      sharesMintedCorrect

    Withdraw burned ->
      burned > 0 &&
      withdrawCorrect burned &&
      sharesBurnedCorrect burned

    Borrow amt ->
      pdTotalLiquidity dat >= amt &&
      borrowRecorded amt

    Repay principal ->
      repayCorrect principal

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput =
      case findOwnInput ctx of
        Just i  -> txInInfoResolved i
        Nothing -> traceError "input missing"

    ownOutput :: TxOut
    ownOutput = getContinuingOutput ctx

    oldAda :: Integer
    oldAda = valueOf (txOutValue ownInput) adaSymbol adaToken

    newAda :: Integer
    newAda = valueOf (txOutValue ownOutput) adaSymbol adaToken

    newDatum :: PoolDatum
    newDatum = getPoolDatum ownOutput

    -------------------------------------------------
    -- DEPOSIT
    -------------------------------------------------

    deposited :: Integer
    deposited = newAda - oldAda

    mintedShares :: Integer
    mintedShares =
      if pdTotalShares dat == 0
        then deposited
        else (deposited * pdTotalShares dat) `divide` oldAda

    depositCorrect :: Bool
    depositCorrect =
      pdTotalLiquidity newDatum == pdTotalLiquidity dat + deposited &&
      pdTotalShares newDatum    == pdTotalShares dat + mintedShares

    sharesMintedCorrect :: Bool
    sharesMintedCorrect =
      assetClassValueOf (txInfoMint info)
        (AssetClass (ownCurrencySymbol ctx, shareTokenName))
        == mintedShares

    -------------------------------------------------
    -- WITHDRAW
    -------------------------------------------------

    withdrawCorrect :: Integer -> Bool
    withdrawCorrect burned =
      let withdrawn = oldAda - newAda
          expected  =
            (burned * oldAda) `divide` pdTotalShares dat
       in withdrawn == expected &&
          pdTotalShares newDatum ==
            pdTotalShares dat - burned

    sharesBurnedCorrect :: Integer -> Bool
    sharesBurnedCorrect burned =
      assetClassValueOf (txInfoMint info)
        (AssetClass (ownCurrencySymbol ctx, shareTokenName))
        == negate burned

    -------------------------------------------------
    -- BORROW
    -------------------------------------------------

    borrowRecorded :: Integer -> Bool
    borrowRecorded amt =
      pdTotalBorrowed newDatum ==
        pdTotalBorrowed dat + amt

    -------------------------------------------------
    -- REPAY
    -------------------------------------------------

    repayCorrect :: Integer -> Bool
    repayCorrect principal =
      let interest =
            (principal * pdInterestRate dat) `divide` 100
       in newAda >= oldAda + principal + interest &&
          pdTotalBorrowed newDatum ==
            pdTotalBorrowed dat - principal

-------------------------------------------------
-- UNTYPED WRAPPER
-------------------------------------------------

{-# INLINABLE mkUntyped #-}
mkUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntyped d r c =
  if mkPoolValidator
      (unsafeFromBuiltinData d)
      (unsafeFromBuiltinData r)
      (unsafeFromBuiltinData c)
  then ()
  else error ()

validator :: Validator
validator =
  mkValidatorScript $
    $$(PlutusTx.compile [|| mkUntyped ||])

-------------------------------------------------
-- SERIALIZATION
-------------------------------------------------

{-# INLINABLE poolValidatorHash #-}
poolValidatorHash :: ValidatorHash
poolValidatorHash =
  let
    bytes :: BS.ByteString
    bytes = LBS.toStrict $ Serialise.serialise validator

    builtin :: BuiltinByteString
    builtin = Builtins.toBuiltin bytes

    hash :: BuiltinByteString
    hash = sha2_256 builtin
  in ValidatorHash hash


sharePolicyScript :: MintingPolicy
sharePolicyScript = sharePolicy poolValidatorHash

main :: IO ()
main = do
  writeCBOR "lending_pool.plutus" validator
  writeCBOR "share_policy.plutus" sharePolicyScript
  putStrLn "Lending Pool + Share Minting Policy compiled"

writeCBOR :: Serialise.Serialise a => FilePath -> a -> IO ()
writeCBOR path script =
  BS.writeFile path $
    B16.encode $
      LBS.toStrict $
        Serialise.serialise script

