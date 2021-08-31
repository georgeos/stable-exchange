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

-- | Onchain code

-- | Script to hold ada
{-# INLINABLE mkValidator #-}
mkValidator :: PubKeyHash -> () -> () -> ScriptContext -> Bool
mkValidator pk () () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info pk

data Holder
instance Scripts.ValidatorTypes Holder where
    type instance DatumType Holder = ()
    type instance RedeemerType Holder = ()

typedValidator :: PubKeyHash -> Scripts.TypedValidator Holder
typedValidator p = Scripts.mkTypedValidator @Holder
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: PubKeyHash -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: PubKeyHash -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

scrAddress :: PubKeyHash -> Ledger.Address
scrAddress = scriptAddress . validator
