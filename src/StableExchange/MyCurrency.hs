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
import           StableExchange.AdaHolder()

-- | Onchain code

-- | Minting policy for MyCurrency
{-# INLINABLE mkPolicy #-}
mkPolicy :: TokenName -> ScriptContext -> Bool
mkPolicy r _ = traceIfFalse "Wrong TokenName" (r == TokenName "MyCoin")

policy :: Scripts.MintingPolicy 
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

mySymbol :: CurrencySymbol
mySymbol = scriptCurrencySymbol policy

-- | Offchain code

-- | Endpoint to mint
type MintSchema = Endpoint "mint" Integer

mint :: Integer -> Contract w MintSchema Text ()
mint amount = do
    let tn = "MyCoin"
        val = Value.singleton mySymbol tn amount
        lookups = Constraints.mintingPolicy policy
        tx      = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData (tn::TokenName))  val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

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
    callEndpoint @"mint" h1 15
    callEndpoint @"mint" h2 10
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 5
    void $ Emulator.waitNSlots 1