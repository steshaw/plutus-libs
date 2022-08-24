{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.MockChain.Monad.Experiment where

import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Language
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tools
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.Alonzo.TxWitness
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.UTxO
import Cardano.Ledger.TxIn
import Cardano.Wallet.Primitive.Slotting
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Cooked.Tx.Constraints.Type
import Data.Array
import Data.Map
import Data.Sequence.Strict
import Data.Set
import GHC.Records

data MockChainSt

mcstUTxO :: MockChainSt -> UTxO (AlonzoEra c)
mcstUTxO = undefined

data MockChainEnv

mceParams :: MockChainEnv -> PParams (AlonzoEra c)
mceParams = undefined

mceEpochInfo :: MockChainEnv -> EpochInfo m
mceEpochInfo = undefined

mceSystemStart :: MockChainEnv -> SystemStart
mceSystemStart = undefined

mceCostModels :: MockChainEnv -> Array Language CostModel
mceCostModels = undefined

data MockChainError where
  MCErrBasicFailure :: BasicFailure c -> MockChainError
  MissingUTxOs :: MockChainError

newtype MockChainT m a = MockChainT
  {unMockChain :: ReaderT MockChainEnv (StateT MockChainSt (ExceptT MockChainError m)) a}
  deriving newtype (Functor, Applicative, Monad, MonadState MockChainSt, MonadError MockChainError, MonadReader MockChainEnv)

{- Note: What 'validateTxSkel' has to do

- generate a transaction

- balance it

  - calculate the correct fee. Think about functions like 'evaluateMinFee' and 'evaluateTransactionFee' here
  -

- validate it

  - staking
  - time range
  - check balance
  - check number of execution units
  -

- update the state of the MockChain accordingly

each of these steps may fail, and then a suitable failure must be signalled.

-}

-- txSkelTxIns :: AlonzoBody era => TxSkel -> Set (TxIn (Crypto era))
-- txSkelTxIns = undefined

txSkelTxIns :: TxSkel -> Set (TxIn era)
txSkelTxIns = undefined

txSkelTxOuts :: TxSkel -> StrictSeq (TxOut era)
txSkelTxOuts = undefined

-- txSkelMint :: TxSkel -> Value era
-- txSkelMint = undefined
txSkelMint :: TxSkel -> Value era
txSkelMint = undefined

-- generateUnbalTxBody :: AlonzoBody era => TxSkel -> TxBody era
-- generateUnbalTxBody skel =
--   TxBody
--     (txSkelTxIns skel) -- inputs
--     undefined -- collateral
--     (txSkelTxOuts skel) -- outputs
--     undefined -- txcerts
--     undefined -- txwdrls
--     undefined -- txfee
--     undefined -- txvldt
--     undefined -- txUpdates
--     undefined -- rqeSignerHashes
--     (txSkelMint skel) -- mint
--     undefined -- scriptIntegrityHash
--     undefined -- adHash
--     undefined -- txnetworkid

-- balanceTx :: Tx era -> Tx era
-- balanceTx = undefined

-- -- validateTxSkelImpl :: TxSkel -> MockChainT m CardanoTx
-- -- validateTxSkelImpl skel = undefined

-- applyValidTxSkel :: Tx era -> MockChainT m TxId
-- applyValidTxSkel = undefined

-- | This is a "guard" function. It throws an error if it is given an unbalanced
-- transaction.
checkBalance :: Core.Tx (AlonzoEra c) -> MockChainT m ()
checkBalance =
  -- somehow use evaluateTransactionBalance
  undefined

-- UTxO is a Map from TxIn to TxOut

-- | All the inputs relevant to the given transaction. This function is probably
-- not necessary. Schould the error it throws be the 'UnknownTxIn' of
-- 'ScriptFailure'?
relevantUTxO ::
  forall c m.
  ( Crypto c,
    Monad m,
    HasField "inputs" (Core.Tx (AlonzoEra c)) (Set (TxIn c))
  ) =>
  Core.Tx (AlonzoEra c) ->
  MockChainT m (UTxO (AlonzoEra c))
relevantUTxO tx = do
  UTxO m <- gets mcstUTxO
  let neededTxIns :: Set (TxIn c)
      neededTxIns = getField @"inputs" tx
  if all (`Data.Map.member` m) neededTxIns
    then return $ UTxO $ restrictKeys m (getField @"inputs" tx)
    else throwError MissingUTxOs

-- why isn't this exported?
type RedeemerReport c = Map RdmrPtr (Either (ScriptFailure c) ExUnits)

checkExecutionUnits ::
  ( Crypto c,
    Monad m,
    HasField "inputs" (Core.Tx (AlonzoEra c)) (Set (TxIn c))
  ) =>
  Core.Tx (AlonzoEra c) ->
  MockChainT m (RedeemerReport c)
checkExecutionUnits tx = do
  params <- asks mceParams
  systemStart <- asks mceSystemStart
  epochInfo <- asks mceEpochInfo
  costModels <- asks mceCostModels
  uTxO <- relevantUTxO tx -- this could just be gets mcstUTxO, I think
  eu <-
    evaluateTransactionExecutionUnits
      params
      tx
      uTxO
      epochInfo
      systemStart
      costModels
  case eu of
    Left bf -> throwError $ MCErrBasicFailure bf
    Right rr -> return rr
