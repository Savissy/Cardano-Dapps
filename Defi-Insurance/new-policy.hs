{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, FilePath, putStrLn, (<>))
import qualified Prelude as P

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import Plutus.V1.Ledger.Value
    ( flattenValue
    , AssetClass(..)
    , assetClassValueOf
    )

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16

--------------------------------------------------------------------------------
-- Minting Policy
--------------------------------------------------------------------------------

{-# INLINABLE mkDocPolicy #-}
mkDocPolicy :: ScriptContext -> Bool
mkDocPolicy ctx =
    traceIfFalse "must mint exactly one invoice NFT" singleMint &&
    traceIfFalse "issuer must sign transaction" issuerSigned &&
    traceIfFalse "NFT not locked at script output" nftLocked
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownCS :: CurrencySymbol
    ownCS = ownCurrencySymbol ctx

    minted :: [(CurrencySymbol, TokenName, Integer)]
    minted = flattenValue (txInfoMint info)

    mintedTokenName :: TokenName
    mintedTokenName =
      case minted of
        [(_, tn, _)] -> tn
        _            -> traceError "invalid mint"

    singleMint :: Bool
    singleMint =
      case minted of
        [(cs, _, amt)] -> cs == ownCS && amt == 1
        _              -> False

    issuerSigned :: Bool
    issuerSigned =
      case txInfoSignatories info of
        [_] -> True
        _   -> False

    -- must lock the minted NFT in exactly ONE script output (any script address)
    nftLocked :: Bool
    nftLocked =
      traceIfFalse "NFT must be locked in exactly one script output" (lockedCount == 1)

    lockedCount :: Integer
    lockedCount =
      foldr (\o acc -> if outputHasNFT o then acc + 1 else acc) 0 (txInfoOutputs info)

    outputHasNFT :: TxOut -> Bool
    outputHasNFT o =
      case txOutAddress o of
        Address (ScriptCredential _) _ ->
          assetClassValueOf (txOutValue o) (AssetClass (ownCS, mintedTokenName)) == 1
        _ -> False

--------------------------------------------------------------------------------
-- Untyped Wrapper
--------------------------------------------------------------------------------

{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> ()
mkPolicy _ ctx =
    if mkDocPolicy (unsafeFromBuiltinData ctx)
    then ()
    else error ()

policy :: MintingPolicy
policy =
    mkMintingPolicyScript
        $$(PlutusTx.compile [|| mkPolicy ||])

--------------------------------------------------------------------------------
-- CBOR Writer
--------------------------------------------------------------------------------

writeCBOR :: FilePath -> MintingPolicy -> IO ()
writeCBOR path val = do
    let bytes = LBS.toStrict (Serialise.serialise val)
    BS.writeFile path (B16.encode bytes)
    putStrLn $ "CBOR hex written to: " <> path

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    writeCBOR "invoice_document_policy.cbor" policy
    putStrLn "--- Invoice Document Minting Policy Compiled ---"
