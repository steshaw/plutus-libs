--
-- WARNING
--
-- Modified version of Ledger.Fee from plutus-apps's plutus-ledger,
-- without `makeAutoBalancedTransaction` and it's auxiliary function, `fromLedgerUTxO`.
--
{-# LANGUAGE ImportQualifiedPost #-}

-- | Calculating transaction fees in the emulator.
module Cooked.MockChain.Fee
  ( estimateTransactionFee,
  )
where

import Cardano.Api.Shelley qualified as C.Api
import Cooked.MockChain.Fees qualified as CookedFees
import Data.Bifunctor (first)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Address (PaymentPubKeyHash)
import Ledger.Params (EmulatorEra, Params (pProtocolParams))
import Ledger.Tx (Tx)
import Ledger.Tx.CardanoAPI (CardanoBuildTx (..), getCardanoBuildTx, toCardanoTxBodyContent)
import Ledger.Validation (CardanoLedgerError, UTxO (..), makeTransactionBody)
import Ledger.Value (Value)

estimateTransactionFee ::
  Params ->
  UTxO EmulatorEra ->
  [PaymentPubKeyHash] ->
  Tx ->
  Either CardanoLedgerError Value
estimateTransactionFee params utxo requiredSigners tx = do
  txBodyContent <- first Right $ toCardanoTxBodyContent params requiredSigners tx
  let nkeys = C.Api.estimateTransactionKeyWitnessCount (getCardanoBuildTx txBodyContent)
  txBody <- makeTransactionBody params utxo txBodyContent
  case CookedFees.evaluateTransactionFee (pProtocolParams params) txBody nkeys 0 of
    C.Api.Lovelace fee -> pure $ lovelaceValueOf fee
