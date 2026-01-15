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
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken, flattenValue)
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
-- Document Minting Policy
--------------------------------------------------------------------------------

{-# INLINABLE mkDocPolicy #-}
mkDocPolicy :: PubKeyHash -> BuiltinByteString -> ScriptContext -> Bool
mkDocPolicy borrower docHash ctx =
    traceIfFalse "borrower not signed" borrowerSigned &&
    traceIfFalse "must mint exactly one NFT" singleMint &&
    traceIfFalse "token name mismatch" correctTokenName
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    borrowerSigned :: Bool
    borrowerSigned = txSignedBy info borrower

    ownSymbol :: CurrencySymbol
    ownSymbol = ownCurrencySymbol ctx

    minted :: [(CurrencySymbol, TokenName, Integer)]
    minted = flattenValue (txInfoMint info)

    singleMint :: Bool
    singleMint =
        case minted of
            [(cs, _, amt)] -> cs == ownSymbol && amt == 1
            _              -> False

    correctTokenName :: Bool
    correctTokenName =
        case minted of
            [(_, tn, _)] -> unTokenName tn == docHash
            _            -> False

--------------------------------------------------------------------------------
-- Untyped Wrapper
--------------------------------------------------------------------------------

{-# INLINABLE mkPolicyUntyped #-}
mkPolicyUntyped :: BuiltinData -> BuiltinData -> ()
mkPolicyUntyped r c =
    let (borrower, docHash) = unsafeFromBuiltinData r
    in if mkDocPolicy borrower docHash (unsafeFromBuiltinData c)
       then ()
       else error ()

policy :: MintingPolicy
policy =
    mkMintingPolicyScript
        $$(PlutusTx.compile [|| mkPolicyUntyped ||])

--------------------------------------------------------------------------------
-- CBOR Hex Generator
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
