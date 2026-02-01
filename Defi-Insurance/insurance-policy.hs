{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx
import Plutus.V1.Ledger.Value (flattenValue)
import PlutusTx.Prelude hiding (Semigroup(..))
import qualified PlutusTx.Builtins as Builtins
import qualified PlutusTx.AssocMap as Map

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-------------------------------------------------
-- GLOBAL SOULBOUND MEMBERSHIP POLICY (TYPED)
-------------------------------------------------

{-# INLINABLE mkMembershipPolicy #-}
mkMembershipPolicy :: () -> ScriptContext -> Bool
mkMembershipPolicy _ ctx =
    traceIfFalse "must mint exactly one NFT" mintExactlyOne &&
    traceIfFalse "must be signed by token owner" signedByOwner
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    mintExactlyOne :: Bool
    mintExactlyOne =
        case flattenValue (txInfoMint info) of
            [(cs, tn, amt)] ->
                cs == ownCurrencySymbol ctx &&
                amt == 1 &&
                tn /= TokenName emptyByteString
            _ -> False

    signedByOwner :: Bool
    signedByOwner =
        case flattenValue (txInfoMint info) of
            [(_, TokenName tn, _)] ->
                txSignedBy info (PubKeyHash tn)
            _ -> False

-------------------------------------------------
-- UNTYPED WRAPPER (REQUIRED)
-------------------------------------------------

{-# INLINABLE mkPolicyUntyped #-}
mkPolicyUntyped :: BuiltinData -> BuiltinData -> ()
mkPolicyUntyped _ ctx =
    if mkMembershipPolicy () (unsafeFromBuiltinData ctx)
    then ()
    else error ()

-------------------------------------------------
-- POLICY
-------------------------------------------------

policy :: MintingPolicy
policy =
    mkMintingPolicyScript
        $$(PlutusTx.compile [|| mkPolicyUntyped ||])

-------------------------------------------------
-- CURRENCY SYMBOL (CORRECT WAY)
-------------------------------------------------

{-# INLINABLE currencySymbol #-}
currencySymbol :: CurrencySymbol
currencySymbol =
    let
        bytes :: BS.ByteString
        bytes = LBS.toStrict $ Serialise.serialise policy  -- strict ByteString

        builtin :: BuiltinByteString
        builtin = Builtins.toBuiltin bytes

        hash :: BuiltinByteString
        hash = sha2_256 builtin
    in CurrencySymbol hash

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
-------------------------------------------------
-- MAIN
-------------------------------------------------

main :: IO ()
main = do

    writeCBOR "insurance_mint_policy.cbor" policy

    putStrLn "\n--- Soulbound Membership Policy ---"
    putStrLn $ "CurrencySymbol: " <> P.show currencySymbol
    putStrLn "---------------------------------"
