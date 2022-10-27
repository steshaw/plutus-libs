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
    ciTxOutFromTxOut,
    minAdaValue,
    txOutValueL,
    babbageTxOut,
    lovelacesIn,
    txOutValueUnsafeI,
    asV2Script,
    singletonConstraint,
    -- re-exports
    (PlutusTx.Numeric.-),
    Cardano.ReferenceScript (..),
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
    Ledger.POSIXTimeRange,
    Ledger.Tx (..),
    Ledger.TxIn (..),
    Ledger.TxInType (..),
    Ledger.TxInput (..),
    Ledger.TxInputType (..), -- what's the deal with TxIn vs TxInput?
    Ledger.TxOut (..),
    Ledger.TxOutRef,
    Ledger.UtxoIndex,
    Ledger.ValidationErrorInPhase,
    Ledger.ValidatorHash (..),
    Ledger.Value,
    Ledger.Value.adaOnlyValue,
    Ledger.Value.isAdaOnlyValue,
    Ledger.Versioned (..),
    Ledger.addSignature',
    Ledger.fromMilliSeconds,
    Ledger.getCardanoTxOutRefs,
    Ledger.getIndex,
    Ledger.increaseTransactionLimits,
    Ledger.initialise,
    Ledger.pubKeyHash,
    Ledger.toTxInfoTxOut,
    Ledger.toTxOut,
    Ledger.txOutAddress,
    Ledger.txOutDatumHash,
    Ledger.txOutPubKey,
    Ledger.txOutValue,
    Plutus.V1.Ledger.Value.adaSymbol,
    Plutus.V1.Ledger.Value.adaToken,
    Plutus.V1.Ledger.Value.flattenValue,
    Plutus.V1.Ledger.Value.geq,
    Plutus.V1.Ledger.Value.leq,
    Plutus.V1.Ledger.Value.lt,
    PlutusTx.FromData,
    PlutusTx.Numeric.negate,
    PlutusTx.fromBuiltinData,
    Scripts.TypedValidator,
    Scripts.validatorAddress,
    Scripts.validatorScript,
    V2Api.Credential (..),
    V2Api.OutputDatum (..),
    Ledger.TokenName,
    (PlutusTx.Prelude.==),
    PlutusTx.Prelude.trace,
    Ledger.ScriptContext (..),
    Ledger.TxInfo (..),
    Ledger.ownCurrencySymbol,
    Ledger.mkMintingPolicyScript,
    Ledger.MintingPolicy,
    PlutusTx.compile,
    Scripts.mkUntypedMintingPolicy,
    PlutusTx.applyCode,
    PlutusTx.liftCode,
    Ledger.scriptCurrencySymbol,
    Ledger.Value.singleton,
    Plutus.V1.Ledger.Value.tokenName,
    Plutus.V1.Ledger.Value.assetClass,
    Plutus.V1.Ledger.Value.assetClassValue,
    Scripts.Language (..),
    Scripts.ValidatorTypes (..),
    PlutusTx.ToData (..),
    PlutusTx.Prelude.Eq,
    Ledger.StakePubKeyHash (..),
    Ledger.AssetClass,
    Ledger.Constraints.ScriptLookups,
    Ledger.Constraints.TxConstraints,
    Ledger.datumHash,
    Ledger.Constraints.otherScript,
    Ledger.Constraints.mustSpendScriptOutput,
    Ledger.Constraints.mustMintValueWithRedeemer,
    Ledger.Constraints.mustMintValue,
    Ledger.Constraints.mustValidateIn,
    Ledger.Constraints.mustBeSignedBy,
    Ledger.Constraints.mustSpendPubKeyOutput,
    Ledger.Constraints.ownStakePubKeyHash,
    Ledger.Constraints.otherData,
    Ledger.Constraints.TxConstraint (..),
    Ledger.PaymentPubKeyHash (..),
    Ledger.Constraints.TxOutDatum (..),
    Ledger.from,
    Ledger.to,
    Ledger.Redeemer (..),
    Scripts.validatorHash,
    Ledger.StakeValidatorHash (..),
    Ledger.Constraints.unspentOutputs,
    Ledger.Constraints.mintingPolicy,
  )
where

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import Data.Either
import qualified Ledger
import qualified Ledger.Ada
import qualified Ledger.CardanoWallet
import qualified Ledger.Constraints
import qualified Ledger.Constraints.TxConstraints
import qualified Ledger.TimeSlot
import qualified Ledger.Tx.CardanoAPI
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value
import Optics.Core
import qualified Plutus.ChainIndex
import qualified Plutus.Contract.CardanoAPI
import qualified Plutus.V1.Ledger.Value
import qualified Plutus.V2.Ledger.Api as V2Api
import qualified PlutusTx
import qualified PlutusTx.Numeric
import qualified PlutusTx.Prelude

-- * Some easy definitions

-- | The minimum Ada amount every UTxO must have, as a 'Value'
minAdaValue :: Ledger.Value
minAdaValue = Ledger.Ada.toValue Ledger.minAdaTxOut

-- | How many Lovelaces are in the given value?
lovelacesIn :: Ledger.Value -> Integer
lovelacesIn v = Ledger.Value.valueOf v Ledger.Ada.adaSymbol Ledger.Ada.adaToken

-- * Working with 'TxOut's

-- | A smart constructor for Babbage era 'TxOut's.
babbageTxOut ::
  Ledger.Address ->
  Ledger.Value ->
  V2Api.OutputDatum ->
  Ledger.ReferenceScript ->
  Either Ledger.ToCardanoError Ledger.TxOut
babbageTxOut addr val dat rScr =
  Ledger.TxOut
    <$> ( Cardano.TxOut
            <$> Ledger.Tx.CardanoAPI.toCardanoAddressInEra
              Cardano.Mainnet -- is this appropriate?
              addr
              <*> ( Cardano.TxOutValue Cardano.MultiAssetInBabbageEra
                      <$> Ledger.Tx.CardanoAPI.toCardanoValue val
                  )
              <*> Ledger.Tx.CardanoAPI.toCardanoTxOutDatum dat
              <*> pure rScr
        )

{- Carl Hammann 7 minutes ago
Ah, I think now I understand: If the reference script is present, it'll not be
just some arbitrary script that has something to say in the transaction under
validation, but the very same script that the output belongs to, right? That
wasn't evident from all of the documentation I read so far.

Jean-Frederic Etienne 6 minutes ago
Yep
  -}
ciTxOutFromTxOut :: Ledger.TxOut -> Maybe Ledger.ChainIndexTxOut
ciTxOutFromTxOut (Ledger.TxOut (Cardano.TxOut cAddr cVal cDat cRScr)) =
  let addr = Ledger.Tx.CardanoAPI.fromCardanoAddressInEra cAddr
      val = Ledger.Tx.CardanoAPI.fromCardanoTxOutValue cVal
      dat = Ledger.Tx.CardanoAPI.fromCardanoTxOutDatum cDat
      -- If the reference script cannot be translated into a 'Versioned Script',
      -- we're not interested, and the translation into a 'ChainIndexTxOut'
      -- should fail.
      rScr :: Maybe (Maybe (Ledger.Versioned Ledger.Script))
      rScr = case Plutus.Contract.CardanoAPI.fromCardanoTxOutRefScript cRScr of
        Plutus.ChainIndex.ReferenceScriptNone -> Just Nothing
        Plutus.ChainIndex.ReferenceScriptInAnyLang sil ->
          case Ledger.Tx.CardanoAPI.fromCardanoScriptInAnyLang sil of
            Nothing -> Nothing
            Just s -> Just $ Just s
   in case addr of
        Ledger.Address (V2Api.PubKeyCredential _) _ ->
          Ledger.PublicKeyChainIndexTxOut
            addr
            val
            ( case dat of
                V2Api.NoOutputDatum -> Nothing
                V2Api.OutputDatumHash dh -> Just (dh, Nothing)
                V2Api.OutputDatum d -> Just (Ledger.datumHash d, Just d)
            )
            <$> rScr
        Ledger.Address (V2Api.ScriptCredential valHash) _ ->
          Ledger.ScriptChainIndexTxOut
            addr
            val
            <$> ( case dat of
                    V2Api.NoOutputDatum -> Nothing
                    V2Api.OutputDatumHash dh -> Just (dh, Nothing)
                    V2Api.OutputDatum d -> Just (Ledger.datumHash d, Just d)
                )
            <*> rScr
            <*> pure (valHash, Nothing)

txOutValueL :: Lens' Ledger.TxOut (Cardano.TxOutValue Cardano.BabbageEra)
txOutValueL = lensVL Ledger.outValue'

-- | There's always a 'Value' to be obtained from a 'TxOutValue BabbageEra'. The
-- converse direction need not always hold. Namely, what can go wrong is
-- 'toCardanoAssetId' throwing an error on one of the 'AssetClass'es in your
-- provided 'Value'. This Iso is safe in both directions if you know at least
-- one of the following conditions to hold:
--
-- - You're only working with pure Ada values.
--
-- - The function 'Cardano.Api.deserialiseFromRawBytes' fails on none of the
--   token names and currency symbols in the values you're working with.
txOutValueUnsafeI :: Iso' (Cardano.TxOutValue Cardano.BabbageEra) Ledger.Value
txOutValueUnsafeI =
  iso
    Ledger.Tx.CardanoAPI.fromCardanoTxOutValue
    ( fromRight (error "Value can not be translated into 'TxOutValue BabbageEra'")
        . Ledger.Tx.CardanoAPI.toCardanoTxOutValue
    )

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

asV2Script :: script -> Scripts.Versioned script
asV2Script = flip Scripts.Versioned Scripts.PlutusV2

singletonConstraint :: Ledger.Constraints.TxConstraint -> Ledger.Constraints.TxConstraints i o
singletonConstraint = Ledger.Constraints.TxConstraints.singleton
