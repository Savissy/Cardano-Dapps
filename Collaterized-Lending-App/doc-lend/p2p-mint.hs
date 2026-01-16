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

data DocRedeemer = DocRedeemer
  { borrower :: PubKeyHash
  , docHash  :: BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''DocRedeemer

--------------------------------------------------------------------------------
-- Document Minting Policy
--------------------------------------------------------------------------------

{-# INLINABLE mkDocPolicy #-}
mkDocPolicy :: DocRedeemer -> ScriptContext -> Bool
mkDocPolicy red ctx =
    traceIfFalse "borrower not signed" borrowerSigned &&
    traceIfFalse "must mint exactly one NFT" singleMint &&
    traceIfFalse "token name mismatch" correctTokenName
  where
    info = scriptContextTxInfo ctx

    borrowerSigned =
        txSignedBy info (borrower red)

    ownSymbol =
        ownCurrencySymbol ctx

    minted =
        flattenValue (txInfoMint info)

    singleMint =
        case minted of
            [(cs, _, amt)] -> cs == ownSymbol && amt == 1
            _              -> False

    correctTokenName =
        case minted of
            [(_, tn, _)] -> unTokenName tn == docHash red
            _            -> False

--------------------------------------------------------------------------------
-- Untyped Wrapper
--------------------------------------------------------------------------------

{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> ()
mkPolicy r ctx =
    if mkDocPolicy (unsafeFromBuiltinData r)
                   (unsafeFromBuiltinData ctx)
    then ()
    else error ()

policy :: MintingPolicy
policy =
    mkMintingPolicyScript
        $$(PlutusTx.compile [|| mkPolicy ||])

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
