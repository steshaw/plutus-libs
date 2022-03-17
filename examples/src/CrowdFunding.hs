{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- A smart contract to model a crowd funding project with a deadline
module CrowdFunding where

-- Plutus related imports
import qualified Ledger
import qualified Ledger.Contexts as Contexts
import qualified PlutusTx
import PlutusTx.Prelude hiding (Applicative (..))

-- Some basic types
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Prelude as Haskell

{-
The datum for the CrowdFunding smart contract. A peer can either :
* propose a project with its address, an id, a goal and a time range
* contribute to a project with its address and an id
-}
data CrowdFundingDatum =
  ProjectProposal Ledger.PubKeyHash BuiltinByteString Integer Ledger.POSIXTimeRange |
Funding Ledger.PubKeyHash BuiltinByteString
  deriving stock    (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Some magic so that Plutus understands that CrowdFundingDatum is supposed to be used as a Datum
PlutusTx.unstableMakeIsData ''CrowdFundingDatum

{-# INLINEABLE findDatumByHash #-}
findDatumByHash :: Contexts.TxInfo -> Ledger.DatumHash -> Maybe CrowdFundingDatum
findDatumByHash txInfo hash = do
  Ledger.Datum d <- Contexts.findDatum hash txInfo
  PlutusTx.fromBuiltinData d

{-# INLINEABLE findDatum #-}
findDatum :: Ledger.TxInfo -> Ledger.TxInInfo -> Maybe CrowdFundingDatum
findDatum txInfo txInInfo =
  Ledger.txOutDatumHash (Ledger.txInInfoResolved txInInfo) >>= findDatumByHash txInfo

-- {-
-- The redeemer for the CrowdFunding smart contract. The project can either be :
-- * launched, with the following parameters :
--   - an AssetClass : the nature of the tokens to give to contributors
--   - a power of 10 : the number of parts in which the project shall be split among contributors
-- * cancelled
-- -}
-- data CrowdFundingRedeemer =
--   Launch AssetClass Integer |
--   Cancel
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON)  

-- {-# INLINEABLE getCurrentValue #-}
-- -- Retrieves the value for the current input
-- getCurrentValue :: ScriptContext -> Maybe Value
-- getCurrentValue = (fmap (txOutValue . txInInfoResolved)) . findOwnInput

-- {-# INLINEABLE validateTimeRange #-}
-- -- Retrieves the current transition time range and compares it to the project time range
-- validateTimeRange :: Ledger.POSIXTimeRange -> ScriptContext -> Bool
-- validateTimeRange range =
--   (== range) .
--   (\/ range) .
--   txInfoValidRange .
--   scriptContextTxInfo

-- {-# INLINEABLE validateTotalSum #-}
-- -- The sum of all input lovelaces should be greater than the threshold
-- validateTotalSum :: Integer -> ScriptContext -> Bool
-- validateTotalSum threshold =
--   (<= threshold) .
--   getLovelace .
--   fromValue .
--   mconcat .
--   fmap (txOutValue . txInInfoResolved) .
--   txInfoInputs .
--   scriptContextTxInfo

-- {-# INLINEABLE isProjectProposal #-}
-- -- Checks if the given Datum corresponds to a project proposal
-- isProjectProposal :: Maybe CrowdFundingDatum -> Bool
-- isProjectProposal (Just (ProjectProposal _ _ _ _)) = True
-- isProjectProposal _ = False

-- {-# INLINEABLE validateSingleProposal #-}
-- -- Checks that the context only contains a single instance of a project proposal
-- validateSingleProposal :: ScriptContext -> Bool
-- validateSingleProposal = undefined
--   -- (== 1) .
--   -- length .
--   -- (filter isProjectProposal) .
--   -- (fmap (PlTx.fromBuiltinData . snd)) .
--   -- txInfoData .
--   -- scriptContextTxInfo
  
-- {-# INLINEABLE validateCrowdFunding #-}
-- validateCrowdFunding :: CrowdFundingDatum -> CrowdFundingRedeemer -> ScriptContext -> Bool
-- -- We can always cancel the project regarding the project proposal, or not
-- -- Make sure there is no other input belonging to the current script when we are in this case
-- validateCrowdFunding (ProjectProposal _ _ _ _) Cancel _   = True
-- -- To launch the project, we need to check if the total amount of money is enough
-- -- We also need to check whether the current time is part of the time range
-- -- Finally, we need to check if there is a single instance of a project proposal
-- validateCrowdFunding (ProjectProposal _ _ threshold range) (Launch _ _) ctx =
--   traceIfFalse "Current time not in the time range."              (validateTimeRange range ctx) &&
--   traceIfFalse "The project has not reach its funding threshold." (validateTotalSum threshold ctx) &&
--   traceIfFalse "There are multiple project proposals."            (validateSingleProposal ctx)
-- validateCrowdFunding (Funding hash id) (Launch _ _) ctx = undefined
-- validateCrowdFunding (Funding hash id) Cancel ctx = undefined

-- data CrowdFunding

-- PlTx.makeLift ''CrowdFundingDatum
-- PlTx.unstableMakeIsData ''CrowdFundingDatum

-- instance Scripts.ValidatorTypes CrowdFunding where
--   type RedeemerType CrowdFunding = CrowdFundingRedeemer
--   type DatumType CrowdFunding = CrowdFundingDatum

-- crowdFundingValidator :: Scripts.TypedValidator CrowdFunding
-- crowdFundingValidator =
--   Scripts.mkTypedValidator @CrowdFunding
--     $$(PlTx.compile [||validateCrowdFunding||])
--     $$(PlTx.compile [||wrap||])
--   where
--     wrap = Scripts.wrapValidator @CrowdFundingDatum @CrowdFundingRedeemer






