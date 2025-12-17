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

import qualified PlutusTx.Builtins as Builtins

import qualified Codec.Serialise as Serialise

import qualified Data.ByteString.Lazy  as LBS

import qualified Data.ByteString.Short as SBS

import qualified Data.ByteString       as BS

import qualified Data.ByteString.Base16 as B16

import qualified Cardano.Api as C

import qualified Cardano.Api.Shelley as CS


---

-- Datum


---

data YieldDatum = YieldDatum

{ ydLender     :: PubKeyHash

, ydBorrower   :: Maybe PubKeyHash   -- ✅ FIX: borrower is optional

, ydPrincipal  :: Integer

, ydInterest   :: Integer

, ydYieldShare :: Integer

}

PlutusTx.unstableMakeIsData ''YieldDatum


---

-- Action


---

data YieldAction

= Deposit

| Borrow Integer

| Repay Integer

| DistributeYield Integer

PlutusTx.unstableMakeIsData ''YieldAction


---

-- Helpers


---

{-# INLINABLE signedBy #-}

signedBy :: PubKeyHash -> ScriptContext -> Bool

signedBy pkh ctx =

txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE anySigner #-}

anySigner :: ScriptContext -> Bool

anySigner ctx =

length (txInfoSignatories (scriptContextTxInfo ctx)) > 0

{-# INLINABLE calcDistribution #-}

calcDistribution :: Integer -> Integer -> (Integer, Integer)

calcDistribution yield ratio =

let lenderPart   = (yield * ratio) `divide` 100

    borrowerPart = yield - lenderPart

in (lenderPart, borrowerPart)

{-# INLINABLE adaPaidTo #-}

adaPaidTo :: TxInfo -> PubKeyHash -> Integer

adaPaidTo info pkh =

valueOf (valuePaidTo info pkh) adaSymbol adaToken

{-# INLINABLE ownInputAda #-}

ownInputAda :: ScriptContext -> Integer

ownInputAda ctx =

case findOwnInput ctx of

    Nothing ->

        traceError "script input missing"



    Just txIn ->

        valueOf

            (txOutValue (txInInfoResolved txIn))

            adaSymbol

            adaToken


---

-- Validator


---

{-# INLINABLE mkYieldValidator #-}

mkYieldValidator :: YieldDatum -> YieldAction -> ScriptContext -> Bool

mkYieldValidator dat action ctx =

case action of



    Deposit ->

        traceIfFalse "lender must sign"

          (signedBy (ydLender dat) ctx)



    Borrow _ ->

        case ydBorrower dat of

            Nothing ->

                -- ✅ First borrower: ANY signer allowed

                traceIfFalse "borrower must sign"

                  (anySigner ctx)



            Just borrower ->

                traceIfFalse "borrower must sign"

                  (signedBy borrower ctx)



    Repay _ ->

        case ydBorrower dat of

            Nothing ->

                traceError "no borrower to repay"



            Just borrower ->

                traceIfFalse "borrower must sign"

                  (signedBy borrower ctx)



    DistributeYield yieldAmt ->

        traceIfFalse "lender must sign"

            (signedBy (ydLender dat) ctx) &&



        case ydBorrower dat of

            Nothing ->

                traceError "no borrower"



            Just borrower ->

                let

                    info = scriptContextTxInfo ctx



                    requiredRepayment =

                        ydPrincipal dat + ydInterest dat



                    scriptAda =

                        ownInputAda ctx



                    (lenderShare, borrowerShare) =

                        calcDistribution yieldAmt (ydYieldShare dat)



                    lenderPaid =

                        adaPaidTo info (ydLender dat)



                    borrowerPaid =

                        adaPaidTo info borrower

                in

                    traceIfFalse "loan not repaid"

                        (scriptAda >= requiredRepayment) &&



                    traceIfFalse "lender not paid correctly"

                        (lenderPaid >= lenderShare) &&



                    traceIfFalse "borrower not paid correctly"

                        (borrowerPaid >= borrowerShare)


---
