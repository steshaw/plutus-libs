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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Fee calculation
module Cooked.MockChain.Fees
  ( evaluateTransactionFee,
    evaluateTransactionFeePParams,
  )
where

import Cardano.Api hiding (evaluateTransactionFee)
import Cardano.Api.Shelley (ProtocolParameters, ShelleyLedgerEra, Tx (ShelleyTx), fromShelleyLovelace)
import Cardano.Ledger.Core (PParams)
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Shelley.API.Wallet qualified as Ledger (evaluateTransactionFee)
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
  case makeSignedTransaction [] txbody of
    ShelleyTx era tx -> evalShelleyBasedEra era tx
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

evaluateTransactionFeePParams ::
  PParams (ShelleyLedgerEra BabbageEra) ->
  TxBody BabbageEra ->
  -- | The number of Shelley key witnesses
  Word ->
  Lovelace
evaluateTransactionFeePParams pparams txbody keywitcount =
  case makeSignedTransaction [] txbody of
    ShelleyTx _ tx -> evalShelleyBasedEra tx
  where
    evalShelleyBasedEra ::
      Ledger.Tx (ShelleyLedgerEra BabbageEra) ->
      Lovelace
    evalShelleyBasedEra tx =
      fromShelleyLovelace $
        Ledger.evaluateTransactionFee
          pparams
          tx
          keywitcount
