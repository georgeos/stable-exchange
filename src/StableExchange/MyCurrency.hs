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
data HolderRedeemer = Redeem | Deposit
    deriving Show

PlutusTx.unstableMakeIsData ''HolderRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: () -> HolderRedeemer -> ScriptContext -> Bool
mkValidator _ r ctx = 
    case r of
        Deposit -> traceIfFalse "Wrong amount to mint" paid
            where
                inputValue :: Value
                inputValue = case findOwnInput ctx of
                    Nothing -> traceError "script validator missing"
                    Just i  -> txOutValue $ txInInfoResolved i

                outputValue :: Value
                outputValue = case getContinuingOutputs ctx of
                    [o] -> txOutValue o
                    _   -> traceError "should be paid to script"

                paid :: Bool
                paid = outputValue `gt` inputValue
        Redeem ->   traceIfFalse "Wrong signature" True

data Holder
instance Scripts.ValidatorTypes Holder where
    type instance DatumType Holder = ()
    type instance RedeemerType Holder = HolderRedeemer

typedValidator :: Scripts.TypedValidator Holder
typedValidator = Scripts.mkTypedValidator @Holder
        $$(PlutusTx.compile [|| mkValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @HolderRedeemer

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
type MintSchema =
        Endpoint "mint" Integer
    .\/ Endpoint "redeem" ()

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
                            Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Deposit)
                        ]
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "minted %s tokens" (show val)
        _     -> do
            let tx = Constraints.mustPayToTheScript () $ Ada.lovelaceValueOf 0
            ledgerTx <- submitTxConstraints typedValidator tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "initialize script validator"

redeem :: Contract w MintSchema Text ()
redeem = do
    utxos <- utxoAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData Redeem | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "redeemed"

endpoints :: Contract () MintSchema Text ()
endpoints = forever
        $ handleError logError
        $ awaitPromise 
        $ mint' `select` redeem'
    where
        mint'   = endpoint @"mint" mint
        redeem' = endpoint @"redeem" $ const redeem

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
    callEndpoint @"mint" h3 15
    void $ Emulator.waitNSlots 1
    callEndpoint @"redeem" h1 ()
    void $ Emulator.waitNSlots 1