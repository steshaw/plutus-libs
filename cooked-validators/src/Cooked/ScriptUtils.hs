-- | This module and its submodules provide utilities to work with Plutus
-- scripts. Most of what we have here is directly copied from plutus-apps. In
-- that case, the name of the function is exactly as it is there.
module Cooked.ScriptUtils
  ( module X,
    mintingPolicyHash,
    scriptCurrencySymbol,
  )
where

import Cooked.ScriptUtils.TypedScripts as X
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value

mintingPolicyHash :: MintingPolicy -> MintingPolicyHash
mintingPolicyHash = undefined

scriptCurrencySymbol :: MintingPolicy -> CurrencySymbol
scriptCurrencySymbol = mpsSymbol . mintingPolicyHash
