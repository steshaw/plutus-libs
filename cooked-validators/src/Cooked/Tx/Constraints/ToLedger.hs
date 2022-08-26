{-# LANGUAGE GADTs #-}

module Cooked.Tx.Constraints.ToLedger where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Mary.Value as MV
import Cardano.Ledger.ShelleyMA.Timelocks
import Cardano.Ledger.Slot
import Cardano.Ledger.TxIn
import Cooked.Tx.Constraints.Optics
import Cooked.Tx.Constraints.Type
import qualified Data.Map as Map
import qualified Data.Set as Set
import Optics.Core
import qualified Plutus.V1.Ledger.Api as Pl
import qualified Plutus.V1.Ledger.Value as Pl
import qualified Test.Cardano.Ledger.Generic.Proof as CT
import qualified Test.Cardano.Ledger.Generic.Updaters as CT

-- I'm using some helpers from cardano-ledger-test to generate
-- transactions. (That's everything that is imported qualified with @CT@). It's
-- probably not a good idea to depend on that for production, but for now, the
-- goal of the following definitions should be to generate the required data for
-- all 'TxBodyField's. If we can accomplish that, I think it would then be quite
-- feasible to generate transactions also without using the 'CT...' interface.

sOutTxIn :: CT.Proof era -> SpendableOut -> TxIn (Crypto era)
sOutTxIn = undefined -- TODO, depending on the actual implemntation of SpendableOut

-- | Extract from a 'TxSkel' all of the 'TxIn's corresponding to UTxOs the
-- transaction should consume.
txInputs :: CT.Proof era -> TxSkel -> Set.Set (TxIn (Crypto era))
txInputs prf =
  foldrOf
    (miscConstraintsL % traversed % spendableOutAT)
    (\o acc -> sOutTxIn prf o `Set.insert` acc)
    mempty

-- | Extract from a 'TxSkel' the interval of slot numbers in which it must
-- validate.
--
-- The @POSIXTime -> SlotNo@ argument should be possible to obtain in the
-- context of a MockChainT.
txValidityInterval :: (Pl.POSIXTime -> SlotNo) -> TxSkel -> ValidityInterval
txValidityInterval timeToEnclosingSlot =
  foldrOf
    (miscConstraintsL % traversed)
    (\mc acc -> maybe acc (intersect acc) (mcValidityInterval mc))
    everything
  where
    intersect :: ValidityInterval -> ValidityInterval -> ValidityInterval
    intersect (ValidityInterval a b) (ValidityInterval x y) =
      ValidityInterval (max' a x) (min' b y)
      where
        max' SNothing j = j
        max' i SNothing = i
        max' (SJust i) (SJust j) = SJust $ max i j

        min' SNothing j = j
        min' i SNothing = i
        min' (SJust i) (SJust j) = SJust $ min i j

    everything :: ValidityInterval
    everything = ValidityInterval SNothing SNothing

    -- If the constraint specifies a time interval (between two 'POSIXTime's),
    -- convert that to 'SlotNo's and return it as a 'ValidityInterval'.
    --
    -- The semantics of the two 'SlotNo's in a 'ValidityInterval' is
    -- "invalidBefore" and "invalidAfter". do we need to use some directed
    -- rounding to convert times to slot numbers?
    mcValidityInterval :: MiscConstraint -> Maybe ValidityInterval
    mcValidityInterval (Before hi) = Just $ ValidityInterval SNothing (SJust $ timeToEnclosingSlot hi)
    mcValidityInterval (After lo) = Just $ ValidityInterval (SJust $ timeToEnclosingSlot lo) SNothing
    -- Does the inclusion of endpoints matter here? I think not, since slots
    -- have positive length.
    mcValidityInterval (ValidateIn (Pl.Interval (Pl.LowerBound lo _loIncluded) (Pl.UpperBound hi _hiIncluded))) =
      case (lo, hi) of
        (Pl.NegInf, Pl.PosInf) -> Just $ ValidityInterval SNothing SNothing
        (Pl.NegInf, Pl.Finite j) -> Just $ ValidityInterval SNothing (SJust $ timeToEnclosingSlot j)
        (Pl.Finite i, Pl.PosInf) -> Just $ ValidityInterval (SJust $ timeToEnclosingSlot i) SNothing
        (Pl.Finite i, Pl.Finite j) -> Just $ ValidityInterval (SJust $ timeToEnclosingSlot i) (SJust $ timeToEnclosingSlot j)
        _ -> Just $ ValidityInterval (SJust 1) (SJust 0) -- the empty interval
    mcValidityInterval _ = Nothing

-- | Extract from a 'TxSkel' its total minted value
txMint :: CT.Proof era -> TxSkel -> Value era
txMint prf = toEraValue prf . foldOf (mintsConstraintsT % valueL)
  where
    -- I'd expect this function to exist somewhere?
    toEraValue :: CT.Proof era -> Pl.Value -> Value era
    toEraValue (CT.Alonzo _) (Pl.Value m) =
      MV.MaryValue
        undefined -- what's this number supposed to be? (I think we'll have to
        -- find out what multiasset values are to answer that question)
        (Map.mapKeys currencySymbolToPolicyId $ Map.map (Map.mapKeys undefined) m)
      where
        currencySymbolToPolicyId :: Pl.CurrencySymbol -> MV.PolicyID c
        currencySymbolToPolicyId = undefined
    toEraValue _ _ = undefined -- it might only be possible to do this for certain eras

toTx :: CT.Proof era -> TxSkel -> Tx era
toTx prf _skel = CT.newTx prf []
