{-# LANGUAGE LambdaCase #-}

-- | This module re-exports and occasionally wraps all of our plutus-apps
-- imports. It should be the ony module in cooked-validators to import anything
-- from plutus-apps or any other dependency related to Plutus or Cardano. This
-- indirection will hopefully make Plutus version updates more local. Also, this
-- module will serve as a single point of truth where we document our
-- assumptions.
module Cooked.PlutusWrappers
  ( -- our own wrappers
    ciTxOutDatum,
    ciTxOutDatumAT,
    ciTxOutDatumHash,
    ciTxOutDatumHashAF,
    fromTxOut,
    minAdaValue,
    txOutValueL,
    -- re-exports
    (PlutusTx.Numeric.-),
    Ledger.Ada.lovelaceValueOf,
    Ledger.Address (..),
    Ledger.CardanoTx (..),
    Ledger.CardanoWallet.MockWallet (..),
    Ledger.CardanoWallet.WalletNumber (..),
    Ledger.CardanoWallet.fromWalletNumber,
    Ledger.CardanoWallet.knownMockWallets,
    Ledger.CardanoWallet.paymentPrivateKey,
    Ledger.CardanoWallet.paymentPubKey,
    Ledger.ChainIndexTxOut (..),
    Ledger.Constraints.MkTxError,
    Ledger.Datum (..),
    Ledger.DatumHash,
    Ledger.DiffMilliSeconds,
    Ledger.OnChainTx (..),
    Ledger.POSIXTime,
    Ledger.Params (..),
    Ledger.PaymentPrivateKey (..),
    Ledger.PaymentPubKey (..),
    Ledger.PubKey,
    Ledger.PubKeyHash,
    Ledger.Script,
    Ledger.Slot (..),
    Ledger.StakingCredential (..),
    Ledger.TimeSlot.SlotConfig,
    Ledger.TimeSlot.posixTimeToEnclosingSlot,
    Ledger.TimeSlot.slotToBeginPOSIXTime,
    Ledger.TimeSlot.slotToEndPOSIXTime,
    Ledger.TimeSlot.slotToPOSIXTimeRange,
    Ledger.Tx (..),
    Ledger.TxIn (..),
    Ledger.TxInType (..),
    Ledger.TxOut (..),
    Ledger.TxOutRef,
    Ledger.UtxoIndex,
    Ledger.ValidationErrorInPhase,
    Ledger.ValidatorHash (..),
    Ledger.Value,
    Ledger.Value.adaOnlyValue,
    Ledger.Value.isAdaOnlyValue,
    Ledger.Versioned,
    Ledger.addSignature',
    Ledger.fromMilliSeconds,
    Ledger.getCardanoTxOutRefs,
    Ledger.getIndex,
    Ledger.increaseTransactionLimits,
    Ledger.initialise,
    Ledger.pubKeyHash,
    Ledger.toTxOut,
    Ledger.txOutAddress,
    Ledger.txOutDatumHash,
    Ledger.txOutPubKey,
    Ledger.txOutValue,
    Plutus.V1.Ledger.Value.adaSymbol,
    Plutus.V1.Ledger.Value.adaToken,
    Plutus.V1.Ledger.Value.flattenValue,
    Plutus.V1.Ledger.Value.leq,
    Plutus.V1.Ledger.Value.lt,
    PlutusTx.FromData,
    PlutusTx.Numeric.negate,
    PlutusTx.fromBuiltinData,
    Scripts.DatumType,
    Scripts.TypedValidator,
    Scripts.validatorAddress,
    V2Api.Credential (..),
  )
where

import qualified Cardano.Api as Cardano
import Data.Either
import qualified Ledger
import qualified Ledger.Ada
import qualified Ledger.CardanoWallet
import qualified Ledger.Constraints
import qualified Ledger.TimeSlot
import qualified Ledger.Tx.CardanoAPI
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value
import Optics.Core
import qualified Plutus.V1.Ledger.Value
import qualified Plutus.V2.Ledger.Api as V2Api
import qualified PlutusTx
import qualified PlutusTx.Numeric

-- * Some easy definitions

-- | The minimum Ada amount every UTxO must have, as a 'Value'
minAdaValue :: Ledger.Value
minAdaValue = Ledger.Ada.toValue Ledger.minAdaTxOut

-- * Working with 'TxOut's

fromTxOut :: Ledger.TxOut -> Maybe Ledger.ChainIndexTxOut
fromTxOut = undefined -- TODO: look at (the documentation at) 'toTxInfoTxOut, which shuold be a left inverse of this function.

-- | There's always a 'Value' in a 'TxOut', at least that's what I hope. I
-- haven't yet understood the conditions under which 'txOutValueI' fails, and if
-- they can happen in our use case.
txOutValueL :: Lens' Ledger.TxOut Ledger.Value
txOutValueL = wrapped % txOutValueI
  where
    wrapped :: Lens Ledger.TxOut Ledger.TxOut Ledger.Value (Cardano.TxOutValue Cardano.BabbageEra)
    wrapped = lensVL Ledger.outValue

txOutValueI :: Iso Ledger.Value (Cardano.TxOutValue Cardano.BabbageEra) Ledger.Value Ledger.Value
txOutValueI =
  iso
    id
    ( fromRight (error "TODO: can this happen?")
        . Ledger.Tx.CardanoAPI.toCardanoTxOutValue
    )

-- | Independenly of whether we have a 'PublicKeyChainIndexTxOut' or a
-- 'ScriptChainIndexTxOut', there's always at most one datum: In the former
-- case, there can be nothing, only a datum hash, or a datum hash together with
-- a datum. In the latter case, there will always be a datum hash, but not
-- always a datum.
--
-- If you use this as a setter, both the datum and the hash will be set, so that
-- the hash is the one of the new datum.
ciTxOutDatumAT :: AffineTraversal' Ledger.ChainIndexTxOut Ledger.Datum
ciTxOutDatumAT =
  atraversal
    ( \case
        Ledger.PublicKeyChainIndexTxOut {Ledger._ciTxOutPublicKeyDatum = Just (_, Just d)} -> Right d
        Ledger.ScriptChainIndexTxOut {Ledger._ciTxOutScriptDatum = (_, Just d)} -> Right d
        x -> Left x
    )
    ( \cio dat ->
        let hash = Ledger.datumHash dat
         in case cio of
              Ledger.PublicKeyChainIndexTxOut {} -> cio {Ledger._ciTxOutPublicKeyDatum = Just (hash, Just dat)}
              Ledger.ScriptChainIndexTxOut {} -> cio {Ledger._ciTxOutScriptDatum = (hash, Just dat)}
    )

ciTxOutDatum :: Ledger.ChainIndexTxOut -> Maybe Ledger.Datum
ciTxOutDatum = (^? ciTxOutDatumAT)

-- | Getting datum hashes should be allowed, but not setting them without
-- setting the datum. This is why this is only a fold.
ciTxOutDatumHashAF :: AffineFold Ledger.ChainIndexTxOut Ledger.DatumHash
ciTxOutDatumHashAF =
  afolding
    ( \case
        Ledger.PublicKeyChainIndexTxOut {Ledger._ciTxOutPublicKeyDatum = Just (dh, _)} -> Just dh
        Ledger.ScriptChainIndexTxOut {Ledger._ciTxOutScriptDatum = (dh, _)} -> Just dh
        _ -> Nothing
    )

ciTxOutDatumHash :: Ledger.ChainIndexTxOut -> Maybe Ledger.DatumHash
ciTxOutDatumHash = (^? ciTxOutDatumHashAF)

{-

txOutPubKeyHashAT :: AffineTraversal' Ledger.TxOut Ledger.PubKeyHash
txOutPubKeyHashAT = txOutAddressL % addressPubKeyHashP

txOutAddressL :: Lens' Ledger.TxOut Ledger.Address
txOutAddressL = wrapped % addressInEraI
  where
    wrapped :: Lens Ledger.TxOut Ledger.TxOut Ledger.Address (Cardano.AddressInEra Cardano.BabbageEra)
    wrapped = lensVL Ledger.outAddress

addressInEraI :: Iso Ledger.Address (Cardano.AddressInEra Cardano.BabbageEra) Ledger.Address Ledger.Address
addressInEraI = iso id undefined -- We could do soemthing like in 'txOutValueI', but that does not seem right, now that it's becoming a pattern.

-- | The "constructing" direction of this prism sets the staking credential to
-- 'Nothing'.
addressPubKeyHashP :: Prism' Ledger.Address Ledger.PubKeyHash
addressPubKeyHashP =
  prism
    (flip Ledger.pubKeyHashAddress Nothing . Ledger.PaymentPubKeyHash) -- TODO is there a more natural way to do this?
    (\addr -> maybe (Left addr) Right (Ledger.toPubKeyHash addr))

-}
