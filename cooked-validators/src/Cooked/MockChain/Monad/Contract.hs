{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Monad.Contract where

import Control.Lens (review)
import Control.Monad
import Cooked.MockChain.Monad
import Cooked.Tx.Constraints
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Void
import qualified Ledger as Pl
import qualified Plutus.Contract as C
import qualified PlutusTx as Pl

-- TODO shall MonadFail really be the constraint on the MonadBlockChain class?
instance (C.AsContractError e) => MonadFail (C.Contract w s e) where
  fail = C.throwError . review C._OtherContractError . T.pack

instance (C.AsContractError e) => MonadBlockChain (C.Contract w s e) where
  validateTxSkel TxSkel {txConstraints, txOpts} = do
    let (lkups, constrs) = toLedgerConstraint @Constraints @Void (toConstraints txConstraints)
    tx <- C.submitTxConstraintsWith lkups constrs
    when (awaitTxConfirmed txOpts) $ C.awaitTxConfirmed $ Pl.getCardanoTxId tx
    return tx

  utxosSuchThat addr datumPred = do
    allUtxos <- M.toList <$> C.utxosAt addr
    maybeUtxosWithDatums <- forM allUtxos $ \utxo -> do
      datum <- datumFromTxOut $ snd utxo
      let typedDatum = datum >>= Pl.fromBuiltinData . Pl.getDatum
      pure $
        if datumPred typedDatum (Pl._ciTxOutValue $ snd utxo)
          then Just (utxo, typedDatum)
          else Nothing
    pure $ catMaybes maybeUtxosWithDatums

  txOutByRef lparams ref = do
    mcout <- C.unspentTxOutFromRef ref
    case mcout of
      Nothing -> pure $ Nothing
      Just cout -> do
        case Pl.toTxOut (Pl.pNetworkId lparams) cout of
          Left err -> C.throwError $ review C._TxToCardanoConvertContractError err
          Right txout -> pure $ Just txout

  ownPaymentPubKeyHash = fmap Pl.unPaymentPubKeyHash C.ownFirstPaymentPubKeyHash

  currentSlot = C.currentPABSlot
  currentTime = C.currentTime
  awaitSlot = C.awaitSlot
  awaitTime = C.awaitTime

  datumFromTxOut (Pl.PublicKeyChainIndexTxOut _ _ Nothing _) = pure $ Nothing
  datumFromTxOut (Pl.PublicKeyChainIndexTxOut _ _ (Just (_, Just d)) _) = pure $ Just d
  -- datum is always present in the nominal case, guaranteed by chain-index
  datumFromTxOut (Pl.PublicKeyChainIndexTxOut _ _ (Just (dh, Nothing)) _) = C.datumFromHash dh
  datumFromTxOut (Pl.ScriptChainIndexTxOut _ _ (_, Just d) _ _) = pure $ Just d
  -- datum is always present in the nominal case, guaranteed by chain-index
  datumFromTxOut (Pl.ScriptChainIndexTxOut _ _ (dh, Nothing) _ _) = C.datumFromHash dh
