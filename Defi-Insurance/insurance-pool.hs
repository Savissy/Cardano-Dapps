{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude (IO, FilePath, putStrLn, seq, (<>))
import qualified Prelude as P

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import Plutus.V1.Ledger.Value
  ( AssetClass(..)
  , assetClassValueOf
  )
import Plutus.V1.Ledger.Api (adaSymbol, adaToken)

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16

--------------------------------------------------------------------------------
-- Datum & Redeemer
--------------------------------------------------------------------------------

data PoolDatum = PoolDatum
    { pdSigners     :: [PubKeyHash]
    , pdThreshold   :: Integer
    , pdShareAsset  :: AssetClass
    , pdTotalShares :: Integer
    }
PlutusTx.unstableMakeIsData ''PoolDatum

data PoolAction
    = Deposit
    | Withdraw Integer
    | ClaimPayout
PlutusTx.unstableMakeIsData ''PoolAction

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINABLE countValidSigners #-}
countValidSigners :: [PubKeyHash] -> [PubKeyHash] -> Integer
countValidSigners allowed actual =
    foldr (\pkh acc -> if elem pkh allowed then acc + 1 else acc) 0 actual

{-# INLINABLE ownInput #-}
ownInput :: ScriptContext -> TxOut
ownInput ctx =
  case findOwnInput ctx of
    Nothing -> traceError "pool: missing own input"
    Just i  -> txInInfoResolved i

{-# INLINABLE onlyOneContinuing #-}
onlyOneContinuing :: [TxOut] -> TxOut
onlyOneContinuing outs =
  case outs of
    [o] -> o
    _   -> traceError "pool: expected exactly one continuing output"

{-# INLINABLE getInlineDatum #-}
getInlineDatum :: TxOut -> PoolDatum
getInlineDatum o =
  case txOutDatum o of
    OutputDatum (Datum d) -> unsafeFromBuiltinData d
    OutputDatumHash _     -> traceError "pool: expected inline datum"
    NoOutputDatum         -> traceError "pool: missing output datum"

{-# INLINABLE firstSigner #-}
firstSigner :: TxInfo -> PubKeyHash
firstSigner info =
  case txInfoSignatories info of
    (pkh:_) -> pkh
    []      -> traceError "pool: at least one signer required"

{-# INLINABLE signerHasShareInput #-}
signerHasShareInput :: TxInfo -> PubKeyHash -> AssetClass -> Integer -> Bool
signerHasShareInput info signer shareAC burned =
  any hasEnoughShareFromSigner (txInfoInputs info)
  where
    hasEnoughShareFromSigner :: TxInInfo -> Bool
    hasEnoughShareFromSigner i =
      case txOutAddress (txInInfoResolved i) of
        Address (PubKeyCredential pkh) _ ->
          pkh == signer &&
          assetClassValueOf (txOutValue (txInInfoResolved i)) shareAC >= burned
        _ -> False

--------------------------------------------------------------------------------
-- Validator Logic
--------------------------------------------------------------------------------

{-# INLINABLE mkPoolValidator #-}
mkPoolValidator :: PoolDatum -> PoolAction -> ScriptContext -> Bool
mkPoolValidator dat action ctx =
    case action of

        -- Anyone can deposit, BUT must mint shares and update pdTotalShares correctly
        Deposit ->
          traceIfFalse "pool: must mint positive shares" (mintedShares > 0) &&
          traceIfFalse "pool: must increase totalShares correctly" totalSharesDepositOk &&
          traceIfFalse "pool: must preserve config" preservesConfig

        -- Anyone can withdraw IF they burn shares, receive proportional ADA, and update totalShares
        Withdraw burnedShares ->
          let
            burnExact :: Bool
            burnExact = mintedShares == negate burnedShares

            signerProvidedShares :: Bool
            signerProvidedShares =
              signerHasShareInput info signer (pdShareAsset dat) burnedShares

            expectedWithdrawAda :: Integer
            expectedWithdrawAda =
              traceIfFalse "pool: zero total shares" (pdTotalShares dat > 0)
                && False
              `seq`
              (burnedShares * inAda) `divide` (pdTotalShares dat)

            paidSignerEnough :: Bool
            paidSignerEnough =
              assetClassValueOf (valuePaidTo info signer) (AssetClass (adaSymbol, adaToken))
                >= expectedWithdrawAda

            poolRemainderOk :: Bool
            poolRemainderOk =
              outAda == (inAda - expectedWithdrawAda)

            totalSharesWithdrawOk :: Bool
            totalSharesWithdrawOk =
              pdTotalShares outDat == pdTotalShares dat - burnedShares
          in
            traceIfFalse "pool: burnedShares must be > 0" (burnedShares > 0) &&
            traceIfFalse "pool: must burn exact shares" burnExact &&
            traceIfFalse "pool: withdraw exceeds total shares" (burnedShares <= pdTotalShares dat) &&
            traceIfFalse "pool: signer must provide share input" signerProvidedShares &&
            traceIfFalse "pool: must pay signer expected ADA" paidSignerEnough &&
            traceIfFalse "pool: pool output ADA must match remainder" poolRemainderOk &&
            traceIfFalse "pool: must decrease totalShares correctly" totalSharesWithdrawOk &&
            traceIfFalse "pool: must preserve config" preservesConfig

        -- Only pool signers can move funds for claim payouts
        ClaimPayout ->
          traceIfFalse "pool: insufficient signatories" thresholdMet

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signer :: PubKeyHash
    signer = firstSigner info

    thresholdMet :: Bool
    thresholdMet =
      countValidSigners (pdSigners dat) (txInfoSignatories info)
        >= pdThreshold dat

    -- Input / output values
    inTxOut :: TxOut
    inTxOut = ownInput ctx

    inAda :: Integer
    inAda = assetClassValueOf (txOutValue inTxOut) (AssetClass (adaSymbol, adaToken))

    contOut :: TxOut
    contOut = onlyOneContinuing (getContinuingOutputs ctx)

    outAda :: Integer
    outAda = assetClassValueOf (txOutValue contOut) (AssetClass (adaSymbol, adaToken))

    outDat :: PoolDatum
    outDat = getInlineDatum contOut

    -- Share minting/burning in THIS transaction
    mintedShares :: Integer
    mintedShares = assetClassValueOf (txInfoMint info) (pdShareAsset dat)

    totalSharesDepositOk :: Bool
    totalSharesDepositOk =
      pdTotalShares outDat == pdTotalShares dat + mintedShares

    preservesConfig :: Bool
    preservesConfig =
      pdSigners outDat     == pdSigners dat     &&
      pdThreshold outDat   == pdThreshold dat   &&
      pdShareAsset outDat  == pdShareAsset dat

--------------------------------------------------------------------------------
-- Untyped Wrapper
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- CBOR
--------------------------------------------------------------------------------

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
    let bytes = LBS.toStrict (Serialise.serialise val)
        hex   = B16.encode bytes
    BS.writeFile path hex
    putStrLn $ "✅ CBOR hex written to: " <> path

main :: IO ()
main = do
    writeCBOR "insurance_pool_multisig.cbor" validator
    putStrLn "✅ Pool Validator compiled"
