{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Typed validators and typed minting policies. This is mostly copied/adapted
-- from plutus-apps.
module Cooked.ScriptUtils.TypedScripts where

import Data.Kind
import Plutus.V1.Ledger.Api
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- * Typed Validators

data TypedValidator a = TypedValidator Validator deriving (Haskell.Eq)

class ValidatorTypes a where
  type RedeemerType a :: Type
  type DatumType a :: Type

type UntypedValidator = BuiltinData -> BuiltinData -> BuiltinData -> ()

{-# INLINEABLE mkUntypedValidator #-}
mkUntypedValidator ::
  (UnsafeFromData d, UnsafeFromData r) =>
  (d -> r -> ScriptContext -> Bool) ->
  UntypedValidator
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
mkUntypedValidator f d r p =
  check $ f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)

-- * Typed Minting Policies

type UntypedMintingPolicy = BuiltinData -> BuiltinData -> ()

{-# INLINEABLE mkUntypedMintingPolicy #-}
mkUntypedMintingPolicy ::
  UnsafeFromData r =>
  (r -> ScriptContext -> Bool) ->
  UntypedMintingPolicy
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
mkUntypedMintingPolicy f r p =
  check $ f (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)
