module Cooked.QuickValueSpec (spec) where

import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Data.Map as Map
import qualified Ledger.Value as Pl
import Test.Hspec

spec :: SpecWith ()
spec = do
  it "make it possible to provide additional assets in the initial state" $
    quickValuesInitialisation
      `shouldSatisfy` isRightAndSatifies
        (hasQuickValueAmount (wallet 1) "goldenCoins" 20)
  it "are exchangeable like any other asset between wallets" $
    paymentAfterCustomInitialization
      `shouldSatisfy` isRightAndSatifies
        (hasQuickValueAmount (wallet 2) "goldenCoins" 12)

customInitialDistribution :: InitialDistribution
customInitialDistribution =
  initialDistribution' [(wallet 1, quickValue "goldenCoins" 20)]

paymentAfterCustomInitialization :: Either MockChainError ((), UtxoState)
paymentAfterCustomInitialization =
  runMockChainFrom customInitialDistribution $ do
    validateTxConstr
      [PaysPK (walletPKHash $ wallet 2) (quickValue "goldenCoins" 12)]
    return ()

quickValuesInitialisation :: Either MockChainError ((), UtxoState)
quickValuesInitialisation = runMockChainFrom customInitialDistribution (return ())

hasQuickValueAmount :: Wallet -> String -> Integer -> UtxoState -> Bool
hasQuickValueAmount wallet quickValueName amount (UtxoState state) =
  case Map.lookup (walletAddress wallet) state of
    Nothing -> False
    Just valuesAndDatums ->
      amount
        == Pl.assetClassValueOf
          (mconcat (map fst valuesAndDatums))
          (quickAssetClass quickValueName)

isRightAndSatifies :: (UtxoState -> Bool) -> Either e (a, UtxoState) -> Bool
isRightAndSatifies property (Right (_, state)) = property state
isRightAndSatifies _ _ = False