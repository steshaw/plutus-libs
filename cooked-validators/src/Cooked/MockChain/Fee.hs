--
-- WARNING
--
-- Modified version of Ledger.Fee from plutus-apps's plutus-ledger,
-- without `makeAutoBalancedTransaction` and it's auxiliary function, `fromLedgerUTxO`.
--
{-# LANGUAGE ImportQualifiedPost #-}

-- | Calculating transaction fees in the emulator.
module Cooked.MockChain.Fee
  ( estimateTransactionFeePParams,
  )
where

import Cardano.Api.Shelley qualified as C.Api
import Cardano.Ledger.Core (PParams)
import Cooked.MockChain.Fees qualified as CookedFees
import Data.Bifunctor (first)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Address (PaymentPubKeyHash)
import Ledger.Params (EmulatorEra, Params)
import Ledger.Tx (Tx)
import Ledger.Tx.CardanoAPI (CardanoBuildTx (..), getCardanoBuildTx, toCardanoTxBodyContent)
import Ledger.Validation (CardanoLedgerError, UTxO (..), makeTransactionBody)
import Ledger.Value (Value)

estimateTransactionFeePParams ::
  PParams (C.Api.ShelleyLedgerEra C.Api.BabbageEra) ->
  Params ->
  UTxO EmulatorEra ->
  [PaymentPubKeyHash] ->
  Tx ->
  Either CardanoLedgerError Value
estimateTransactionFeePParams pparams params utxo requiredSigners tx = do
  txBodyContent <- first Right $ toCardanoTxBodyContent params requiredSigners tx
  let nkeys = C.Api.estimateTransactionKeyWitnessCount (getCardanoBuildTx txBodyContent)
  txBody <- makeTransactionBody params utxo txBodyContent
  case CookedFees.evaluateTransactionFeePParams pparams txBody nkeys of
    C.Api.Lovelace fee -> pure $ lovelaceValueOf fee
