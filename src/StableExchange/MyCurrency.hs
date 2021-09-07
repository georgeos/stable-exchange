{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module StableExchange.MyCurrency where

import           Control.Monad          hiding (fmap)
import           Data.Text              (Text)
import           Data.Void              (Void)
import qualified Data.Map               as Map
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import           Ledger.Ada             as Ada
import           GHC.Base               ((<>))

-- | Onchain code

-- | Script to hold ada
{-# INLINABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = True

data Holder
instance Scripts.ValidatorTypes Holder where
    type instance DatumType Holder = ()
    type instance RedeemerType Holder = ()

typedValidator :: Scripts.TypedValidator Holder
typedValidator = Scripts.mkTypedValidator @Holder
        $$(PlutusTx.compile [|| mkValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- | Minting policy for MyCurrency
{-# INLINABLE mkPolicy #-}
mkPolicy :: Address -> TokenName -> ScriptContext -> Bool
mkPolicy address r ctx =
    let
        info :: TxInfo
        info = scriptContextTxInfo ctx

        inputsInfo :: [TxInInfo]
        inputsInfo = txInfoInputs info

        hasUTxOFromScript :: Bool
        hasUTxOFromScript = any (\input -> txOutAddress (txInInfoResolved input) == address) inputsInfo
    in
    traceIfFalse "Hasn't been deposited" hasUTxOFromScript &&
    traceIfFalse "Wrong TokenName" (r == TokenName "MyCoin")

policy :: Address -> Scripts.MintingPolicy
policy address = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode address

mySymbol :: Address -> CurrencySymbol
mySymbol = scriptCurrencySymbol . policy

-- | Offchain code

-- | Endpoint to mint
type MintSchema = Endpoint "mint" Integer

mint :: Integer -> Contract w MintSchema Text ()
mint amount = do
    utxos <- utxoAt scrAddress
    case Map.toList utxos of
        [(oref, o)] -> do
            let tn = "MyCoin"
                val = Value.singleton (mySymbol scrAddress) tn amount
                ada = amount * 1000000
                lookups =
                        Constraints.unspentOutputs (Map.singleton oref o) <>
                        Constraints.otherScript validator <>
                        Constraints.mintingPolicy (policy scrAddress)
                tx      = mconcat [
                            Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData ()) (Ada.lovelaceValueOf ada <> txOutValue (txOutTxOut o)),
                            Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData (tn::TokenName))  val,
                            Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ())
                        ]
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "minted %s tokens" (show val)
        _     -> do
            let tx = Constraints.mustPayToTheScript () $ Ada.lovelaceValueOf 0
            ledgerTx <- submitTxConstraints typedValidator tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "initialize script validator"

endpoints :: Contract () MintSchema Text ()
endpoints = forever
        $ handleError logError
        $ awaitPromise mint'
    where mint' = endpoint @"mint" mint

-- | Trace
test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"mint" h1 0
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h2 10
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h3 5
    void $ Emulator.waitNSlots 1