{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken, AssetClass(..))
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
-- DATUM (UPDATED)
-------------------------------------------------

data InsuranceDatum = InsuranceDatum
    { idThreshold   :: Integer
    , idClaimerPkh  :: PubKeyHash     -- NEW (replaces Maybe claimant)
    , idClaimAmount :: Integer
    , idVotes       :: [PubKeyHash]
    , idDocAsset    :: AssetClass     -- NEW (doc NFT asset class)
    , idExecuted    :: Bool           -- NEW
    }
PlutusTx.unstableMakeIsData ''InsuranceDatum

-------------------------------------------------
-- REDEEMER
-------------------------------------------------

data InsuranceAction
    = PayPremium
    | SubmitClaim Integer
    | VoteClaim
    | ExecuteClaim
PlutusTx.unstableMakeIsData ''InsuranceAction

-------------------------------------------------
-- SOULBOUND NFT CONFIG (GLOBAL)
-------------------------------------------------

{-# INLINABLE membershipPolicy #-}
membershipPolicy :: CurrencySymbol
membershipPolicy =
    CurrencySymbol
      "3f6f56667f1e1c683316e9f5616c3d9383f8045057764c103a042c6fbc47d131"

-------------------------------------------------
-- HELPERS
-------------------------------------------------

{-# INLINABLE signerHasMembership #-}
signerHasMembership :: TxInfo -> PubKeyHash -> Bool
signerHasMembership info pkh =
    let tn = TokenName (getPubKeyHash pkh)
    in
        valueOf (txInfoMint info) membershipPolicy tn == 0
        &&
        any
            (\i ->
                valueOf (txOutValue $ txInInfoResolved i) membershipPolicy tn >= 1
            )
            (txInfoInputs info)

{-# INLINABLE hasSignerInput #-}
hasSignerInput :: TxInfo -> PubKeyHash -> Bool
hasSignerInput info pkh =
    any
        (\i ->
            case txOutAddress (txInInfoResolved i) of
                Address (PubKeyCredential pkh') _ -> pkh' == pkh
                _ -> False
        )
        (txInfoInputs info)

-------------------------------------------------
-- VALIDATOR
-------------------------------------------------

{-# INLINABLE mkInsuranceValidator #-}
mkInsuranceValidator :: InsuranceDatum -> InsuranceAction -> ScriptContext -> Bool
mkInsuranceValidator dat action ctx =
    case action of

        PayPremium ->
            traceIfFalse "membership required" memberAuth

        SubmitClaim amt ->
            traceIfFalse "membership required" memberAuth &&
            traceIfFalse "invalid amount" (amt > 0)

        VoteClaim ->
            traceIfFalse "membership required" memberAuth &&
            traceIfFalse "double vote" noDoubleVote

        ExecuteClaim ->
            traceIfFalse "threshold not met" thresholdMet &&
            traceIfFalse "claimant not paid" claimantPaid

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -------------------------------------------------
    -- SINGLE SIGNER ENFORCEMENT
    -------------------------------------------------

    signer :: PubKeyHash
    signer =
        case txInfoSignatories info of
            [pkh] -> pkh
            _     -> traceError "exactly one signer required"

    memberAuth :: Bool
    memberAuth =
        txSignedBy info signer
        && hasSignerInput info signer
        && signerHasMembership info signer

    noDoubleVote :: Bool
    noDoubleVote =
        not (elem signer (idVotes dat))

    thresholdMet :: Bool
    thresholdMet =
        length (idVotes dat) >= idThreshold dat

    claimantPaid :: Bool
    claimantPaid =
        valueOf (valuePaidTo info (idClaimerPkh dat)) adaSymbol adaToken
            >= idClaimAmount dat

-------------------------------------------------
-- UNTYPED WRAPPER
-------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkInsuranceValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator =
    mkValidatorScript
        $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-------------------------------------------------
-- ADDRESS / SERIALISATION
-------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes  = Serialise.serialise val
        short  = SBS.toShort (LBS.toStrict bytes)
    in PlutusV2.ValidatorHash (toBuiltin (SBS.fromShort short))

plutusScriptAddress :: Address
plutusScriptAddress =
    Address
        (ScriptCredential (plutusValidatorHash validator))
        Nothing

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
-- FILE OUTPUT
-------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
    let bytes = LBS.toStrict (Serialise.serialise val)
    BS.writeFile path (B16.encode bytes)
    putStrLn $ "CBOR hex written to: " <> path

-------------------------------------------------
-- MAIN
-------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "insurance.plutus" validator
    writeCBOR      "insurance.cbor"   validator

    putStrLn "\n--- Insurance Pool (Soulbound Membership) ---"
    putStrLn $ "Validator Hash: " <> P.show (plutusValidatorHash validator)
    putStrLn $ "Bech32 Address: " <> toBech32ScriptAddress network validator
    putStrLn "--------------------------------------------"
