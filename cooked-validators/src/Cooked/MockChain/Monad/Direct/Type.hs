{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cooked.MockChain.Monad.Direct.Type where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

data MockChainEnv

data MockChainSt

data MockChainError

-- | The actual 'MockChainT' is a trivial combination of 'StateT' and 'ExceptT'
newtype MockChainT m a = MockChainT
  {unMockChain :: ReaderT MockChainEnv (StateT MockChainSt (ExceptT MockChainError m)) a}
  deriving newtype (Functor, Applicative, Monad, MonadState MockChainSt, MonadError MockChainError, MonadReader MockChainEnv)
