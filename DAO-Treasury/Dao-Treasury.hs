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
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-------------------------------------------------
-- DATUM
-------------------------------------------------

data TreasuryDatum = TreasuryDatum
    { tdMembers   :: [PubKeyHash]
    , tdQuorum    :: Integer
    , tdRecipient :: PubKeyHash
    , tdAmount    :: Integer        -- ADA to release
    , tdVotes     :: [PubKeyHash]   -- YES votes
    }
PlutusTx.unstableMakeIsData ''TreasuryDatum

-------------------------------------------------
-- REDEEMER
-------------------------------------------------

data TreasuryAction
    = Propose
    | Vote
    | Execute
PlutusTx.unstableMakeIsData ''TreasuryAction

-------------------------------------------------
-- HELPERS
-------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx =
    txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE isMember #-}
isMember :: PubKeyHash -> [PubKeyHash] -> Bool
isMember pkh members =
    elem pkh members

-------------------------------------------------
-- VALIDATOR
-------------------------------------------------

{-# INLINABLE mkTreasuryValidator #-}
mkTreasuryValidator :: TreasuryDatum -> TreasuryAction -> ScriptContext -> Bool
mkTreasuryValidator dat action ctx =
    case action of

        -------------------------------------------------
        -- Proposal creation
        -------------------------------------------------
        Propose ->
            traceIfFalse "treasury must remain locked" treasuryPreserved

        -------------------------------------------------
        -- Voting
        -------------------------------------------------
        Vote ->
            traceIfFalse "not DAO member" voterIsMember &&
            traceIfFalse "not signed" voterSigned       &&
            traceIfFalse "double vote" noDoubleVote

        -------------------------------------------------
        -- Execute proposal
        -------------------------------------------------
        Execute ->
            traceIfFalse "quorum not met" quorumMet     &&
            traceIfFalse "recipient not paid" paidOut

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -------------------------------------------------
    -- Voter
    -------------------------------------------------
    voter :: PubKeyHash
    voter =
        case txInfoSignatories info of
            [pkh] -> pkh
            _     -> traceError "exactly one signer required"

    voterSigned :: Bool
    voterSigned = signedBy voter ctx

    voterIsMember :: Bool
    voterIsMember = isMember voter (tdMembers dat)

    noDoubleVote :: Bool
    noDoubleVote = not (elem voter (tdVotes dat))

    -------------------------------------------------
    -- Quorum
    -------------------------------------------------
    quorumMet :: Bool
    quorumMet =
        length (tdVotes dat) >= tdQuorum dat

    -------------------------------------------------
    -- Treasury safety
    -------------------------------------------------
    treasuryPreserved :: Bool
    treasuryPreserved =
        case findOwnInput ctx of
            Nothing -> traceError "no script input"
            Just i  ->
                let v = txOutValue (txInInfoResolved i)
                in valueOf v adaSymbol adaToken > 0

    -------------------------------------------------
    -- Payment
    -------------------------------------------------
    paidOut :: Bool
    paidOut =
        let paid =
                valueOf
                    (valuePaidTo info (tdRecipient dat))
                    adaSymbol
                    adaToken
        in paid >= tdAmount dat

-------------------------------------------------
-- UNTYPED WRAPPER
-------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkTreasuryValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator =
    mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-------------------------------------------------
-- HASH & ADDRESS
-------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes = Serialise.serialise val
        short = SBS.toShort (LBS.toStrict bytes)
    in PlutusV2.ValidatorHash (toBuiltin (SBS.fromShort short))

plutusScriptAddress :: Address
plutusScriptAddress =
    Address
        (ScriptCredential (plutusValidatorHash validator))
        Nothing

-------------------------------------------------
-- BECH32 ADDRESS (OFF-CHAIN)
-------------------------------------------------

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash   = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

-------------------------------------------------
-- CBOR
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
-- FILE WRITERS
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
-- MAIN
-------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "dao_treasury.plutus" validator
    writeCBOR      "dao_treasury.cbor"   validator

    let bech32 = toBech32ScriptAddress network validator
        cborH  = validatorToCBORHex validator

    putStrLn "\n--- DAO Treasury (No Deadlines) ---"
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn $ "CBOR (first 120 chars): " <> P.take 120 cborH <> "..."
    putStrLn "----------------------------------"
