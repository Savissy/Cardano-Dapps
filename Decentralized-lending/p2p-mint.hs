{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, FilePath, putStrLn, (<>), String)
import qualified Prelude as P

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import Plutus.V1.Ledger.Value (flattenValue)

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16

--------------------------------------------------------------------------------
-- Document NFT Minting Policy (NO REDEEMER)
--------------------------------------------------------------------------------

{-# INLINABLE mkDocPolicy #-}
mkDocPolicy :: ScriptContext -> Bool
mkDocPolicy ctx =
    traceIfFalse "must mint exactly one document NFT" singleMint &&
    traceIfFalse "borrower must sign transaction" signerPresent
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownSymbol :: CurrencySymbol
    ownSymbol = ownCurrencySymbol ctx

    minted :: [(CurrencySymbol, TokenName, Integer)]
    minted = flattenValue (txInfoMint info)

    -- Exactly one NFT minted under this policy
    singleMint :: Bool
    singleMint =
        case minted of
            [(cs, _, amt)] -> cs == ownSymbol && amt == 1
            _              -> False

    -- Whoever submits must sign (borrower = wallet owner)
    signerPresent :: Bool
    signerPresent =
        case txInfoSignatories info of
            [_] -> True
            _   -> False

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
-- CBOR HEX GENERATOR
--------------------------------------------------------------------------------

policyToCBORHex :: MintingPolicy -> String
policyToCBORHex val =
    let bytes = LBS.toStrict $ Serialise.serialise val
    in BS.foldr (\b acc -> byteToHex b <> acc) "" bytes
  where
    hexChars = "0123456789abcdef"
    byteToHex b =
        let hi = P.fromIntegral b `P.div` 16
            lo = P.fromIntegral b `P.mod` 16
        in [ hexChars P.!! hi, hexChars P.!! lo ]

writeCBOR :: FilePath -> MintingPolicy -> IO ()
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
    writeCBOR "document_policy.cbor" policy
    putStrLn "\n--- Document NFT Minting Policy Compiled ---"
