module Cooked.MockChain.Constraints where

import Cooked.MockChain.Monad
import qualified Cooked.PlutusWrappers as Pl
import Cooked.Tx.Constraints.Type

-- ** Some supplementary constraints, relying on being in the monad.

-- | Enforces the transaction to be vadiated at a precise slot.
-- It requires to be in the mock chain monad, since slots can be translated to an explicit time range
-- only after inspecting the slot configuration.
validateAtSlot :: MonadMockChain m => Pl.Slot -> m MiscConstraint
validateAtSlot s = do
  slotConf <- slotConfig
  return $ ValidateIn $ Pl.slotToPOSIXTimeRange slotConf s

-- | Validates the transaction in the current time slot of the mock chain.
validateNow :: MonadMockChain m => m MiscConstraint
validateNow =
  validateAtSlot =<< currentSlot
