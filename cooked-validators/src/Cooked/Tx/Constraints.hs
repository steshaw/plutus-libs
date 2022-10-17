{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Tx.Constraints
  ( module Cooked.Tx.Constraints.Type,
    module Cooked.Tx.Constraints.Optics,
    module Cooked.Tx.Constraints.Pretty,
    LedgerConstraint,
    extractDatumStr,
    signedByWallets,
    toChainIndexTxOut,
    toLedgerConstraint,
    mkTxOut,
    orderTxOutputs,
  )
where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Optics
import Cooked.Tx.Constraints.Pretty
import Cooked.Tx.Constraints.Type
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Ledger as Pl hiding (singleton, unspentOutputs, validatorHash)
import qualified Ledger.Constraints as Pl
import qualified Ledger.Constraints.TxConstraints as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Tx.CardanoAPI as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, RedeemerType, validatorScript)
import qualified Plutus.Script.Utils.Scripts as PlU
import qualified Plutus.Script.Utils.V1.Scripts as PV1
import qualified Plutus.V2.Ledger.Api as PV2
import qualified PlutusTx as Pl

-- * Converting 'Constraint's to 'Pl.ScriptLookups', 'Pl.TxConstraints'

type LedgerConstraint a =
  (Pl.ScriptLookups a, Pl.TxConstraints (Pl.RedeemerType a) (Pl.DatumType a))

-- | Convenience class for common operations on what can be converted to
-- 'LedgerConstraint' (in other words native Plutus `Pl.TxConstraints`). This
-- covers output constraints 'OutConstraint', miscelaneous constraints
-- 'MiscConstraint', and combinations of both within 'Constraints'.
--
-- As a user, you should not have to deal with or worry about this class and
-- stick with the 'ConstraintsSpec' instances to specify constraints in
-- transaction skeletons 'TxSkel'.
class ToLedgerConstraint constraint where
  -- | Map from datum hashes to string representation of all the datums carried.
  -- We use this in order to display data to the use when testing. Its often
  -- easier to read the original datatype that was placed into a UTxO
  -- instead of its respective @toBuilinData@ image.
  extractDatumStr :: constraint -> M.Map Pl.DatumHash String

  -- | Converts our constraint into a 'LedgerConstraint',
  --  which later can be used to generate a transaction. The universally
  --  quantified type-variable is there on purpose, to enable us to
  --  easily spend from multiple scripts at the same time.
  toLedgerConstraint :: constraint -> LedgerConstraint a

instance ToLedgerConstraint MiscConstraint where
  extractDatumStr (SpendsScript _ _ (_, Pl.ScriptChainIndexTxOut _ _ (_, Just datum) _ _)) =
    M.singleton (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData datum) (show datum)
  extractDatumStr _ = M.empty

  toLedgerConstraint (SpendsScript v r (oref, o)) = (lkups, constr)
    where
      lkups =
        Pl.plutusV1OtherScript (Pl.validatorScript v)
          <> Pl.unspentOutputs (M.singleton oref o)
      constr = Pl.mustSpendScriptOutput oref (Pl.Redeemer $ Pl.toBuiltinData r)
  toLedgerConstraint (SpendsPK (oref, o)) = (lkups, constr)
    where
      lkups = Pl.unspentOutputs (M.singleton oref o)
      constr = Pl.mustSpendPubKeyOutput oref
  toLedgerConstraint (Mints Nothing pols v) = (lkups, constr)
    where
      lkups = foldMap Pl.plutusV1MintingPolicy pols
      constr = Pl.mustMintValue v
  toLedgerConstraint (Mints (Just r) pols v) = (lkups, constr)
    where
      lkups = foldMap Pl.plutusV1MintingPolicy pols
      constr = Pl.mustMintValueWithRedeemer (Pl.Redeemer (Pl.toBuiltinData r)) v
  toLedgerConstraint (Before t) = (mempty, constr)
    where
      constr = Pl.mustValidateIn (Pl.to t)
  toLedgerConstraint (After t) = (mempty, constr)
    where
      constr = Pl.mustValidateIn (Pl.from t)
  toLedgerConstraint (ValidateIn r) = (mempty, Pl.mustValidateIn r)
  toLedgerConstraint (SignedBy hashes) = (mempty, foldMap (Pl.mustBeSignedBy . Pl.PaymentPubKeyHash) hashes)

instance ToLedgerConstraint OutConstraint where
  extractDatumStr (PaysScript _validator datum _value) =
    M.singleton (PlU.datumHash . Pl.Datum . Pl.toBuiltinData $ datum) (show datum)
  extractDatumStr (PaysPKWithDatum _pk _stak mdat _v) =
    maybe M.empty (\d -> M.singleton (PlU.datumHash . Pl.Datum $ Pl.toBuiltinData d) (show d)) mdat

  toLedgerConstraint (PaysPKWithDatum p stak dat v) = (lkups, constr)
    where
      mData = fmap (Pl.Datum . Pl.toBuiltinData) dat

      lkups =
        maybe mempty Pl.otherData mData
          -- TODO: do we want to akk ownStakePubKeyHash on 'PaysPKWithDatum'? Would we rather have
          -- a different 'WithOwnStakePubKeyHash' constraint?
          <> maybe mempty Pl.ownStakePubKeyHash stak
      constr = Pl.singleton $ Pl.MustPayToPubKeyAddress (Pl.PaymentPubKeyHash p) stak (fmap Pl.TxOutDatumInTx mData) Nothing v
  toLedgerConstraint (PaysScript v datum value) = (lkups, constr)
    where
      lkups = Pl.plutusV1OtherScript (Pl.validatorScript v)
      constr =
        -- use of this constraint to properly add entry in datum witness map.
        -- TODO: need to enrich PaysScript constraint to consider only datumHash (datum not added in map),
        -- DatumInTx and InlineDatum
        Pl.mustPayToOtherScriptWithDatumInTx
          (PV1.validatorHash $ Pl.validatorScript v)
          (Pl.Datum $ Pl.toBuiltinData datum)
          value

instance ToLedgerConstraint Constraints where
  extractDatumStr (miscConstraints :=>: outConstraints) =
    M.union
      (M.unions (extractDatumStr <$> miscConstraints))
      (M.unions (extractDatumStr <$> outConstraints))

  toLedgerConstraint (miscConstraints :=>: outConstraints) =
    (mconcat lkups, mconcat constrs)
    where
      (lkups, constrs) =
        unzip $
          (toLedgerConstraint <$> miscConstraints)
            <> (toLedgerConstraint <$> outConstraints)

-- | Converts a Pl.TxOut to a tuple (Pl.ChainIndexTxOut, Maybe Pl.datum)
-- If the datum witness map is provided the datum is resolved.
-- Otherwise only the datum hash is provided in the corresponding ChainIndexTxOut.
toChainIndexTxOut :: Pl.TxOut -> Maybe (M.Map Pl.DatumHash Pl.Datum) -> (Pl.ChainIndexTxOut, Maybe Pl.Datum)
toChainIndexTxOut (Pl.TxOut (C.TxOut addr val datum refScript)) mdatum =
  let v = Pl.fromCardanoValue $ C.txOutValueToValue val
      d = Pl.fromCardanoTxOutDatum datum
      rs = toVersionedScript refScript
      addr' = Pl.fromCardanoAddressInEra addr
   in case (Pl.addressCredential addr') of
        Pl.PubKeyCredential _ ->
          let !pkDatum = toPubKeyDatum d
           in (Pl.PublicKeyChainIndexTxOut addr' v pkDatum rs, maybe Nothing snd pkDatum)
        Pl.ScriptCredential vh ->
          let !scrDatum = toScriptDatum d
           in (Pl.ScriptChainIndexTxOut addr' v scrDatum rs (vh, toVersionedValidator rs), snd scrDatum)
  where
    toVersionedScript :: C.ReferenceScript era -> Maybe (Pl.Versioned Pl.Script)
    toVersionedScript C.ReferenceScriptNone = Nothing
    toVersionedScript (C.ReferenceScript _ script) = Pl.fromCardanoScriptInAnyLang script

    toVersionedValidator :: Maybe (Pl.Versioned Pl.Script) -> Maybe (Pl.Versioned Pl.Validator)
    toVersionedValidator Nothing = Nothing
    toVersionedValidator (Just (PlU.Versioned s lang)) = (Just $ Pl.Versioned (Pl.Validator s) lang)

    toScriptDatum :: PV2.OutputDatum -> (Pl.DatumHash, Maybe Pl.Datum)
    toScriptDatum PV2.NoOutputDatum = error "toChainIndexTxOut: script datum expected !!!"
    toScriptDatum (PV2.OutputDatumHash dh) = (dh, maybe Nothing (M.lookup dh) mdatum)
    toScriptDatum (PV2.OutputDatum d) = (Pl.datumHash d, Just d)

    toPubKeyDatum :: PV2.OutputDatum -> Maybe (Pl.DatumHash, Maybe Pl.Datum)
    toPubKeyDatum PV2.NoOutputDatum = Nothing
    toPubKeyDatum (PV2.OutputDatumHash dh) = Just (dh, maybe Nothing (M.lookup dh) mdatum)
    toPubKeyDatum (PV2.OutputDatum d) = Just (Pl.datumHash d, Just d)

-- | makes a `Pl.TxOut` with respected to the 'default' ledger params
--  Raise an error if TxOut cannot be created.
mkTxOut :: Pl.Params -> Pl.Address -> Pl.Value -> Maybe (Pl.TxOutDatum Pl.Datum) -> Pl.ReferenceScript -> Pl.TxOut
mkTxOut lparams addr v mTxOutDatum refScript =
  let cardanoTxOut =
        fmap Pl.TxOut $
          C.TxOut <$> Pl.toCardanoAddressInEra (Pl.pNetworkId lparams) addr
            <*> Pl.toCardanoTxOutValue v
            <*> pure (toTxOutDatum mTxOutDatum)
            <*> pure refScript
   in case cardanoTxOut of
        Left err -> error $ "mkTxOut: " ++ show err
        Right cTxOut -> cTxOut
  where
    toTxOutDatum :: Maybe (Pl.TxOutDatum Pl.Datum) -> C.TxOutDatum C.CtxTx C.BabbageEra
    toTxOutDatum Nothing = Pl.toCardanoTxOutNoDatum
    toTxOutDatum (Just (Pl.TxOutDatumHash d)) = Pl.toCardanoTxOutDatumHashFromDatum d
    toTxOutDatum (Just (Pl.TxOutDatumInTx d)) = Pl.toCardanoTxOutDatumInTx d
    toTxOutDatum (Just (Pl.TxOutDatumInline d)) = Pl.toCardanoTxOutDatumInline d

-- | Generate the 'Pl.TxOut' transaction output associated to a given output
-- constraint 'OutConstraint' w.r.t. a given networkId
outConstraintToTxOut :: Pl.Params -> OutConstraint -> Pl.TxOut
outConstraintToTxOut lparams outCstr =
  let (_, lcstr) = toLedgerConstraint outCstr
   in case head $ Pl.txConstraints lcstr of
        Pl.MustPayToPubKeyAddress pk skhM mdv Nothing v ->
          -- No reference script expected
          let addr = Pl.pubKeyHashAddress pk skhM
           in mkTxOut lparams addr v mdv C.ReferenceScriptNone
        Pl.MustPayToOtherScript vlh svhM dv Nothing v ->
          -- No reference script expected
          let addr = Pl.scriptValidatorHashAddress vlh svhM
           in mkTxOut lparams addr v (Just dv) C.ReferenceScriptNone
        _ -> error "outConstraintToTxOut: unexpected output constraint"

-- | Reorders the outputs of a transaction according to the ordered list of
-- output constraints that generate them. Fails in case of mismatch. The
-- reordered outputs are put at the beginning of the list.
-- orderTxOutputs :: MonadFail m => [OutConstraint] -> [Pl.TxOut] -> m [Pl.TxOut]
-- -- TODO Check staking credentials
-- orderTxOutputs [] txOuts = return txOuts
-- orderTxOutputs (oc : ocs) txOuts =
--   case findGeneratedTxOutput oc txOuts of
--     Nothing ->
--       fail $
--         "Could not locate output corresponding to constraint "
--           <> show (prettyOutConstraint oc)
--     Just txOut -> (txOut :) <$> orderTxOutputs ocs (List.delete txOut txOuts)
orderTxOutputs :: Pl.Params -> [OutConstraint] -> [Pl.TxOut] -> [Pl.TxOut]
orderTxOutputs lparams expected given =
  let res = map (outConstraintToTxOut lparams) expected
   in res ++ (given List.\\ res)

-- | @signedByWallets ws == SignedBy $ map walletPKHash ws@
signedByWallets :: [Wallet] -> MiscConstraint
signedByWallets = SignedBy . map walletPKHash
