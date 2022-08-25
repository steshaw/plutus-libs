module Cooked.Tx.Constraints.ToLedger where

-- I'm using some helpers from cardano-ledger-test to generate
-- transactions. It's probably not a good idea to depend on that. For now...

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import Cardano.Ledger.ShelleyMA.Timelocks
import Cardano.Ledger.TxIn
import Cooked.Tx.Constraints.Optics
import Cooked.Tx.Constraints.Type
import Data.Set
import Optics.Core
import Plutus.V1.Ledger.Api
import qualified Test.Cardano.Ledger.Generic.Proof as CT
import qualified Test.Cardano.Ledger.Generic.Updaters as CT

sOutTxIn :: CT.Proof era -> SpendableOut -> TxIn (Crypto era)
sOutTxIn = undefined -- TODO, depending on the actual implemntation of SpendableOut

txInputs :: CT.Proof era -> TxSkel -> Set (TxIn (Crypto era))
txInputs prf =
  foldrOf
    (miscConstraintsL % traversed % spendableOutAT)
    (\o acc -> sOutTxIn prf o `insert` acc)
    mempty

txValidityInterval :: TxSkel -> ValidityInterval
txValidityInterval =
  foldrOf
    (miscConstraintsL % traversed)
    (\mc acc -> maybe acc (intersect acc) (mcValidityInterval mc))
    everything
  where
    intersect :: ValidityInterval -> ValidityInterval -> ValidityInterval
    intersect (ValidityInterval a b) (ValidityInterval x y) =
      ValidityInterval (max' a x) (min' b y)
      where
        max' SNothing SNothing = SNothing
        max' SNothing j = j
        max' i SNothing = i
        max' (SJust i) (SJust j) = SJust $ max i j

        min' SNothing SNothing = SNothing
        min' SNothing j = j
        min' i SNothing = i
        min' (SJust i) (SJust j) = SJust $ min i j

    everything :: ValidityInterval
    everything = ValidityInterval SNothing SNothing

    -- The semantics of the two 'SlotNo's in a 'ValidityInterval' is
    -- "invalidBefore" and "invalidAfter". do we need to use some directed
    -- rounding to convert times to slotNumbers?
    mcValidityInterval :: MiscConstraint -> Maybe ValidityInterval
    mcValidityInterval (Before hi) = Just $ ValidityInterval SNothing (SJust $ firstSlotContaining hi)
    mcValidityInterval (After lo) = Just $ ValidityInterval (SJust $ lastSlotContaining lo) SNothing
    mcValidityInterval (ValidateIn (Interval lo hi)) =
      Just $
        ValidityInterval (SJust $ lastSlotContaining lo) (SJust $ firstSlotContaining hi)
    mcValidityInterval _ = Nothing

    -- these'll probably additional parameters like 'SystemStart'? TODO
    firstSlotContaining = undefined
    lastSlotContaining = undefined

toTx :: CT.Proof era -> TxSkel -> Tx era
toTx prf _skel = CT.newTx prf []
