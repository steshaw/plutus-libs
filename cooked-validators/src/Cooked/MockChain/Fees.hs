--
-- WARNING
--
-- Modified version of Cardano.Api.Fees.evaluateTransactionFee
-- from cardano-nodes's cardano-api.
--
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Fee calculation
module Cooked.MockChain.Fees
  ( evaluateTransactionFee,
  )
where

import Cardano.Api hiding (BalancedTxBody, MinimumUTxOError, ScriptErrorEvaluationFailed, ScriptErrorExecutionUnitsOverflow, ScriptErrorMissingCostModel, ScriptErrorMissingScript, ScriptErrorMissingTxIn, ScriptErrorNotPlutusWitnessedTxIn, ScriptErrorRedeemerPointsToUnknownScriptHash, ScriptErrorTxInWithoutDatum, ScriptErrorWrongDatum, ScriptExecutionError, TransactionValidityCostModelError, TransactionValidityError, TransactionValidityIntervalError, TransactionValidityTranslationError, TxBodyError, TxBodyErrorAdaBalanceNegative, TxBodyErrorAdaBalanceTooSmall, TxBodyErrorAssetBalanceWrong, TxBodyErrorAutoBalance, TxBodyErrorByronEraNotSupported, TxBodyErrorMinUTxOMissingPParams, TxBodyErrorMinUTxONotMet, TxBodyErrorMissingParamMinUTxO, TxBodyErrorNonAdaAssetsUnbalanced, TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap, TxBodyErrorValidityInterval, TxBodyScriptBadScriptValidity, TxBodyScriptExecutionError, calculateMinimumUTxO, estimateTransactionFee, estimateTransactionKeyWitnessCount, evaluateTransactionBalance, evaluateTransactionExecutionUnits, evaluateTransactionFee, mapTxScriptWitnesses, transactionFee) --(IsShelleyBasedEra, Tx, Lovelace (Lovelace), ShelleyBasedEra (..), ByronEra, NetworkId, TxBody, TxBodyContent (TxBodyContent), BuildTx)
import Cardano.Api.Shelley (ProtocolParameters, ShelleyLedgerEra, Tx (ShelleyTx), fromShelleyLovelace)
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Shelley.API qualified as Ledger (CLI)
import Cardano.Ledger.Shelley.API.Wallet qualified as Ledger (evaluateTransactionFee)
import System.IO.Unsafe (unsafePerformIO)
import Prelude

-- | Compute the transaction fee for a proposed transaction, with the
-- assumption that there will be the given number of key witnesses (i.e.
-- signatures).
--
-- TODO: we need separate args for Shelley vs Byron key sigs
evaluateTransactionFee ::
  ProtocolParameters ->
  TxBody BabbageEra ->
  -- | The number of Shelley key witnesses
  Word ->
  Lovelace
evaluateTransactionFee pparams txbody keywitcount =
  seq logEvaluateTransactionFee $
  case makeSignedTransaction [] txbody of
    ShelleyTx era tx -> withLedgerConstraints era (evalShelleyBasedEra era tx)
  where
    evalShelleyBasedEra ::
      ShelleyBasedEra BabbageEra ->
      Ledger.Tx (ShelleyLedgerEra BabbageEra) ->
      Lovelace
    evalShelleyBasedEra era tx =
      fromShelleyLovelace $
        Ledger.evaluateTransactionFee
          (babbageToLedgerPParams era pparams)
          tx
          keywitcount

    babbageToLedgerPParams :: ShelleyBasedEra BabbageEra -> ProtocolParameters -> Ledger.PParams (ShelleyLedgerEra BabbageEra)
    babbageToLedgerPParams = toLedgerPParams

    -- Conjure up all the necessary class instances and evidence
    withLedgerConstraints ::
      ShelleyLedgerEra era ~ ledgerera =>
      ShelleyBasedEra era ->
      ( Ledger.CLI ledgerera =>
        a
      ) ->
      a
    withLedgerConstraints ShelleyBasedEraShelley f = f
    withLedgerConstraints ShelleyBasedEraAllegra f = f
    withLedgerConstraints ShelleyBasedEraMary f = f
    withLedgerConstraints ShelleyBasedEraAlonzo f = f
    withLedgerConstraints ShelleyBasedEraBabbage f = f

    logEvaluateTransactionFee = unsafePerformIO $ putStrLn "\nevaluateTransactionFee!"

