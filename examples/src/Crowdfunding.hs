{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- These language extensions are just what Split.hs uses

-- | Arrange a crowdfund with a deadline and threshold
module Crowdfunding where

import qualified Ledger as L
import qualified Ledger.Interval as Interval
import Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified PlutusTx
import qualified PlutusTx.Numeric as L
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- * Data types

-- | All the data associated with crowdfunding that the validator needs to know
data ValParams = ValParams
  { -- | project must be funded by this time
    projectDeadline :: L.POSIXTime,
    -- | amount that must be reached for project to be funded
    threshold :: L.Value,
    -- | minimum contribution that can be made
    minContribution :: L.Value,
    -- | address to be paid to if threshold is reached by deadline
    fundingTarget :: L.PubKeyHash,
    -- | asset class of the reward tokens
    rewardTokenAssetClass :: Value.AssetClass
  }
  deriving (Haskell.Show)

PlutusTx.makeLift ''ValParams
PlutusTx.unstableMakeIsData ''ValParams

instance Eq ValParams where
  {-# INLINEABLE (==) #-}
  ValParams pd t mc ft ac == ValParams pd' t' mc' ft' ac' =
    pd == pd' && t == t' && mc == mc' && ft == ft' && ac == ac'

-- | All data the minting policy of the reward token needs to
-- know. These are known after the project funding transaction
newtype PolicyParams = PolicyParams
  { -- | TokenName of the reward token
    pRewardTokenName :: Value.TokenName
  }

PlutusTx.makeLift ''PolicyParams
PlutusTx.unstableMakeIsData ''PolicyParams

-- | Datum type. Either project proposal with policy params
-- or funding from address to address with value contributed
data Datum
  = Proposal ValParams
  | Funding L.PubKeyHash L.PubKeyHash L.Value
  deriving (Haskell.Show)

PlutusTx.makeLift ''Datum
PlutusTx.unstableMakeIsData ''Datum

instance Eq Datum where
  {-# INLINEABLE (==) #-}
  Proposal vp == Proposal vp' = vp == vp'
  Funding to from val == Funding to' from' val' = to == to' && from == from' && val == val'
  _ == _ = False

{-# INLINEABLE getOwner #-}
getOwner :: Datum -> L.PubKeyHash
getOwner (Proposal vp) = fundingTarget vp
getOwner (Funding _ to _) = to

{-# INLINEABLE getFunder #-}
getFunder :: Datum -> Maybe L.PubKeyHash
getFunder (Funding from _ _) = Just from
getFunder _ = Nothing

{-# INLINEABLE getValParams #-}
getValParams :: Datum -> Maybe ValParams
getValParams (Proposal vp) = Just vp
getValParams _ = Nothing

-- | Retrieve funder from 'Funding' datum and owner from 'Proposal' datum
{-# INLINEABLE getFunderOwner #-}
getFunderOwner :: Datum -> L.PubKeyHash
getFunderOwner (Proposal vp) = fundingTarget vp
getFunderOwner (Funding from _ _) = from

{-# INLINEABLE getValue #-}
getValue :: Datum -> Maybe L.Value
getValue (Funding _ _ val) = Just val
getValue _ = Nothing

-- | Actions to be taken in the crowdfund. This will be the 'RedeemerType'
data Action
  = -- | Pay funds to owner and reward contributors or (if after deadline) refund everyone
    Launch
  | -- | Refund contributors
    IndividualRefund
  deriving (Haskell.Show)

PlutusTx.makeLift ''Action
PlutusTx.unstableMakeIsData ''Action

instance Eq Action where
  {-# INLINEABLE (==) #-}
  Launch == Launch = True
  IndividualRefund == IndividualRefund = True
  _ == _ = False

-- * The minting policy of the reward token

-- | This minting policy controls the reward tokens of a crowdfund. There are n tokens
-- minted during the project's launch, where n is the number of contributors. It's valid if
-- * exactly n tokens are minted
-- * all contributors receive one token + amount contributed - amount in funding datum
--   + note: this result must be at least 2 ada. if not, transaction will fail earlier
{-# INLINEABLE mkPolicy #-}
mkPolicy :: PolicyParams -> () -> L.ScriptContext -> Bool
mkPolicy (PolicyParams tName) _ ctx
  | amnt == Just (length contributors) =
    traceIfFalse
      "Not all contributors receive token + leftover value"
      (all validContribution contributors)
  | otherwise = trace "not minting the right amount" False
  where
    txi = L.scriptContextTxInfo ctx
    contributors = getUniqueContributors txi
    L.Minting me = L.scriptContextPurpose ctx
    token :: L.Value
    token = Value.singleton me tName 1

    amnt :: Maybe Integer
    amnt = case Value.flattenValue (L.txInfoMint txi) of
      [(cs, tn, a)] | cs == me && tn == tName -> Just a
      _ -> Nothing

    validContribution :: L.PubKeyHash -> Bool
    validContribution addr =
      let receives = receivesFrom txi
          inputs = L.txInfoInputs txi
          inputsAddr =
            filter
              (\i -> L.toPubKeyHash (L.txOutAddress $ L.txInInfoResolved i) == Just addr)
              inputs
          inputsTotal = sum $ map (L.txOutValue . L.txInInfoResolved) inputsAddr
          datums = getAllDatums txi
          funderDatums = filter (\x -> getFunder x == Just addr) datums
          datumTotal = getTotalValue funderDatums
       in addr `receives` (token <> inputsTotal <> L.negate datumTotal)

{-# INLINEABLE rewardTokenName #-}
rewardTokenName :: L.PubKeyHash -> Value.TokenName
rewardTokenName (L.PubKeyHash bs) = Value.TokenName bs

-- | Parameterized minting policy
rewardTokenPolicy :: PolicyParams -> Scripts.MintingPolicy
rewardTokenPolicy pars =
  L.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode pars

getRewardTokenAssetClass :: L.PubKeyHash -> Value.AssetClass
getRewardTokenAssetClass ft =
  Value.assetClass
    (L.scriptCurrencySymbol $ rewardTokenPolicy $ PolicyParams $ rewardTokenName ft)
    (rewardTokenName ft)

{-# INLINEABLE crowdfundTimeRange #-}
crowdfundTimeRange :: ValParams -> L.POSIXTimeRange
crowdfundTimeRange a = Interval.to (projectDeadline a)

-- | Extract a datum state from an output (if it has one)
{-# INLINEABLE outputDatumState #-}
outputDatumState :: L.TxInfo -> L.TxOut -> Maybe Datum
outputDatumState txi o = do
  h <- L.txOutDatum o
  L.Datum d <- L.findDatum h txi
  PlutusTx.fromBuiltinData d

-- | Test that the value paid to the given public key address is at
-- least the given value
{-# INLINEABLE receivesFrom #-}
receivesFrom :: L.TxInfo -> L.PubKeyHash -> L.Value -> Bool
receivesFrom txi who what = L.valuePaidTo txi who `Value.geq` what

-- | Get list of all datums consumed by a transaction
{-# INLINEABLE getAllDatums #-}
getAllDatums :: L.TxInfo -> [Datum]
getAllDatums txi =
  mapMaybe (outputDatumState txi) (map L.txInInfoResolved $ L.txInfoInputs txi)

-- | Get *unique* contributors from a transaction
{-# INLINEABLE getUniqueContributors #-}
getUniqueContributors :: L.TxInfo -> [L.PubKeyHash]
getUniqueContributors txi = nub $ mapMaybe getFunder $ getAllDatums txi

-- | Get total value contributed from a list of datums
{-# INLINEABLE getTotalValue #-}
getTotalValue :: [Datum] -> L.Value
getTotalValue = sum . mapMaybe getValue

-- | Check if the refunding of a particular address at least some amount is valid.
-- It is valid if
-- * the transaction is signed by the address being refunded
-- * all inputs of the transaction point to the original funder
-- * the output points to the original funder
-- * contributor is refunded the amount contributed
{-# INLINEABLE validIndividualRefund #-}
validIndividualRefund :: L.PubKeyHash -> L.ScriptContext -> Bool
validIndividualRefund addr ctx =
  let txi = L.scriptContextTxInfo ctx
      inputs = map L.txInInfoResolved $ L.txInfoInputs txi
      inputAddrs = mapMaybe L.txOutPubKey inputs
      outputAddrs = mapMaybe L.txOutPubKey $ L.txInfoOutputs txi
      receives = receivesFrom txi
   in traceIfFalse
        "Transaction not signed by contributor"
        (txi `L.txSignedBy` addr)
        && traceIfFalse
          "List of input addresses is not only the person being refunded"
          (inputAddrs == [addr])
        && traceIfFalse
          "List of output addresses is not only the person being refunded"
          (nub outputAddrs == [addr])
        && traceIfFalse
          "Contributor is not refunded correct amount"
          (addr `receives` (L.valueSpent txi - L.txInfoFee txi))

-- | Launch after the deadline is valid if
-- * the owner signs the transaction
{-# INLINEABLE validAllRefund #-}
validAllRefund :: ValParams -> L.ScriptContext -> Bool
validAllRefund cf ctx =
  let txi = L.scriptContextTxInfo ctx
   in traceIfFalse
        "Transaction not signed by owner"
        (txi `L.txSignedBy` fundingTarget cf)

-- | Launch before the deadline is valid if
-- * it occurs before the deadline
-- * the total sum of all contributions is greater than the threshold
-- * the funding target is paid the funds
{-# INLINEABLE validLaunch #-}
validLaunch :: ValParams -> L.ScriptContext -> Bool
validLaunch cf ctx =
  let txi = L.scriptContextTxInfo ctx
      receives = receivesFrom txi
      total = getTotalValue $ getAllDatums txi
   in traceIfFalse
        "Contributions after the deadline are not permitted"
        (crowdfundTimeRange cf `Interval.contains` L.txInfoValidRange txi)
        && traceIfFalse
          "Total contributions do not exceed threshold"
          (total `Value.geq` threshold cf)
        && traceIfFalse
          "Funding target not paid total contributions"
          (fundingTarget cf `receives` (total - L.txInfoFee txi))

-- | An individual contributing during launch is valid if
-- * the owner signs the transaction
-- * the contribution is at least the minimum bid
{-# INLINEABLE validFund #-}
validFund :: L.PubKeyHash -> L.PubKeyHash -> L.Value -> L.ScriptContext -> Bool
validFund _ to _ ctx =
  let txi = L.scriptContextTxInfo ctx
      datums = getAllDatums txi
      allParams = mapMaybe getValParams datums
      [currParams] = filter (\vp -> fundingTarget vp == to) allParams
      Just input = L.findOwnInput ctx
      value = L.txOutValue $ L.txInInfoResolved input
   in traceIfFalse
        "Transaction not signed by owner"
        (txi `L.txSignedBy` to)
        && traceIfFalse
          "Contribution is not at least the minimum value"
          (value `Value.geq` minContribution currParams)

{-# INLINEABLE validate #-}
validate :: Datum -> Action -> L.ScriptContext -> Bool
validate (Funding from to val) Launch ctx =
  validFund from to val ctx
validate (Proposal cf) Launch ctx
  | validRange = validLaunch cf ctx
  | otherwise = validAllRefund cf ctx
  where
    txi = L.scriptContextTxInfo ctx
    validRange = crowdfundTimeRange cf `Interval.contains` L.txInfoValidRange txi
validate (Funding from _ _) IndividualRefund ctx =
  validIndividualRefund from ctx
validate (Proposal _) IndividualRefund _ =
  -- disallowed, though this could be allowing the owner to cancel the project
  -- before the deadline, which is currently impossible
  traceIfFalse "Proposal datum with individual refund action not allowed" False

data Crowdfunding

instance Scripts.ValidatorTypes Crowdfunding where
  type RedeemerType Crowdfunding = Action
  type DatumType Crowdfunding = Datum

crowdfundingValidator :: Scripts.TypedValidator Crowdfunding
crowdfundingValidator =
  Scripts.mkTypedValidator @Crowdfunding
    $$(PlutusTx.compile [||validate||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Datum @Action
