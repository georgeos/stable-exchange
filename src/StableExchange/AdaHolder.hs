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

module StableExchange.AdaHolder where

import           Control.Monad          hiding (fmap)
import           Data.Text              (Text)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Prelude                (IO, String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import           Ledger.Ada             as Ada

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

-- | Offchain code

type GiftSchema =
    Endpoint "give" Integer

give :: Integer -> Contract w GiftSchema Text ()
give amount = do
    let ada = amount * 1000000
        tx = mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData ()) $ Ada.lovelaceValueOf ada
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d ada" ada

endpointsHolder :: Contract () GiftSchema Text ()
endpointsHolder = forever
    $ handleError logError
    $ awaitPromise give'
    where
        give' = endpoint @"give" give

-- | Trace
test1 :: IO ()
test1 = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet 1) endpointsHolder
    h2 <- activateContractWallet (Wallet 2) endpointsHolder
    callEndpoint @"give" h1 15
    callEndpoint @"give" h2 20
    void $ Emulator.waitNSlots 1