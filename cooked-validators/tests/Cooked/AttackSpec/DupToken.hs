{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.AttackSpec.DupToken (tests) where

import Control.Monad
import Cooked.Attack
import Cooked.AttackSpec.Util
import Cooked.Currencies
import Cooked.Ltl
import Cooked.MockChain
import qualified Cooked.PlutusWrappers as Pl
import Cooked.Tx.Constraints
import Data.Default
import Test.Tasty
import Test.Tasty.HUnit

{-# INLINEABLE mkCarefulPolicy #-}
mkCarefulPolicy :: Pl.TokenName -> Integer -> () -> Pl.ScriptContext -> Bool
mkCarefulPolicy tName allowedAmount _ ctx
  | amnt Pl.== Just allowedAmount = True
  | otherwise = Pl.trace "tried to mint wrong amount" False
  where
    txi = Pl.scriptContextTxInfo ctx

    amnt :: Maybe Integer
    amnt = case Pl.flattenValue (Pl.txInfoMint txi) of
      [(cs, tn, a)] | cs Pl.== Pl.ownCurrencySymbol ctx && tn Pl.== tName -> Just a
      _ -> Nothing

carefulPolicy :: Pl.TokenName -> Integer -> Pl.Versioned Pl.MintingPolicy
carefulPolicy tName allowedAmount =
  flip Pl.Versioned Pl.PlutusV2 $
    Pl.mkMintingPolicyScript $
      $$(Pl.compile [||\n x -> Pl.mkUntypedMintingPolicy (mkCarefulPolicy n x)||])
        `Pl.applyCode` Pl.liftCode tName
        `Pl.applyCode` Pl.liftCode allowedAmount

{-# INLINEABLE mkCarelessPolicy #-}
mkCarelessPolicy :: () -> Pl.ScriptContext -> Bool
mkCarelessPolicy _ _ = True

carelessPolicy :: Pl.Versioned Pl.MintingPolicy
carelessPolicy =
  flip Pl.Versioned Pl.PlutusV2 $
    Pl.mkMintingPolicyScript
      $$(Pl.compile [||Pl.mkUntypedMintingPolicy mkCarelessPolicy||])

dupTokenTrace :: MonadBlockChain m => Pl.Versioned Pl.MintingPolicy -> Pl.TokenName -> Integer -> Wallet -> m ()
dupTokenTrace pol tName amount recipient = void $ validateTxSkel skel
  where
    skel =
      txSkelOpts (def {adjustUnbalTx = True}) $
        let minted = Pl.singleton (Pl.scriptCurrencySymbol pol) tName amount
         in [Mints (Nothing @()) [pol] minted]
              :=>: [paysPK (walletPKHash recipient) minted]

tests :: TestTree
tests =
  testGroup
    "token duplication attack"
    [ testGroup "unit tests on a 'TxSkel'" $
        let attacker = wallet 6
            tName1 = Pl.tokenName "MockToken1"
            tName2 = Pl.tokenName "MockToken2"
            pol1 = carefulPolicy tName1 1
            pol2 = carelessPolicy
            ac1 = Pl.assetClass (Pl.scriptCurrencySymbol pol1) tName1
            ac2 = Pl.assetClass (Pl.scriptCurrencySymbol pol2) tName2
            skelIn =
              txSkel
                ( [ Mints (Nothing @()) [pol1, pol2] (Pl.assetClassValue ac1 1 <> Pl.assetClassValue ac2 1),
                    Mints (Nothing @()) [pol2] (Pl.assetClassValue ac2 3),
                    Mints (Nothing @()) [pol1] (Pl.assetClassValue ac1 7)
                  ]
                    :=>: [ paysPK (walletPKHash (wallet 1)) (Pl.assetClassValue ac1 1 <> Pl.lovelaceValueOf 1234),
                           paysPK (walletPKHash (wallet 2)) (Pl.assetClassValue ac2 2)
                         ]
                )
            skelOut select = getTweak (dupTokenAttack select attacker) def skelIn
            skelExpected v1 v2 v3 v4 =
              [ ( txSkelLbl
                    DupTokenLbl
                    ( [ Mints (Nothing @()) [pol1, pol2] (Pl.assetClassValue ac1 v1 <> Pl.assetClassValue ac2 v2),
                        Mints (Nothing @()) [pol2] (Pl.assetClassValue ac2 v3),
                        Mints (Nothing @()) [pol1] (Pl.assetClassValue ac1 v4)
                      ]
                        :=>: [ paysPK (walletPKHash (wallet 1)) (Pl.assetClassValue ac1 1 <> Pl.lovelaceValueOf 1234),
                               paysPK (walletPKHash (wallet 2)) (Pl.assetClassValue ac2 2),
                               paysPK
                                 (walletPKHash attacker)
                                 (Pl.assetClassValue ac1 ((v1 - 1) + (v4 - 7)) <> Pl.assetClassValue ac2 ((v2 - 1) + (v3 - 3)))
                             ]
                    ),
                  Pl.assetClassValue ac1 ((v1 - 1) + (v4 - 7)) <> Pl.assetClassValue ac2 ((v2 -1) + (v3 -3))
                )
              ]
         in [ testCase "add one token in every asset class" $ skelExpected 2 2 4 8 @=? skelOut (\_ n -> n + 1),
              testCase "no modified transaction if no increase in value specified" $ [] @=? skelOut (\_ n -> n),
              testCase "add tokens depending on the asset class" $ skelExpected 6 1 3 12 @=? skelOut (\ac n -> if ac == ac1 then n + 5 else n)
            ],
      testCase "careful minting policy" $
        let tName = Pl.tokenName "MockToken"
            pol = carefulPolicy tName 1
         in testFailsFrom'
              isCekEvaluationFailure
              def
              ( somewhere
                  (dupTokenAttack (\_ n -> n + 1) (wallet 6))
                  (dupTokenTrace pol tName 1 (wallet 1))
              ),
      testCase "careless minting policy" $
        let tName = Pl.tokenName "MockToken"
            pol = carelessPolicy
         in testSucceeds $
              somewhere
                (dupTokenAttack (\_ n -> n + 1) (wallet 6))
                (dupTokenTrace pol tName 1 (wallet 1)),
      testCase "pre-existing tokens are left alone" $
        let attacker = wallet 6
            pol = carelessPolicy
            ac1 = Pl.assetClass (Pl.scriptCurrencySymbol pol) (Pl.tokenName "mintedToken")
            ac2 = quickAssetClass "preExistingToken"
            skelIn =
              txSkel
                ( [Mints (Nothing @()) [pol] (Pl.assetClassValue ac1 1)]
                    :=>: [ paysPK
                             (walletPKHash (wallet 1))
                             (Pl.assetClassValue ac1 1 <> Pl.assetClassValue ac2 2)
                         ]
                )
            skelExpected =
              [ ( txSkelLbl
                    DupTokenLbl
                    ( [Mints (Nothing @()) [pol] (Pl.assetClassValue ac1 2)]
                        :=>: [ paysPK
                                 (walletPKHash (wallet 1))
                                 (Pl.assetClassValue ac1 1 <> Pl.assetClassValue ac2 2),
                               paysPK
                                 (walletPKHash attacker)
                                 (Pl.assetClassValue ac1 1)
                             ]
                    ),
                  Pl.assetClassValue ac1 1
                )
              ]
            skelOut = getTweak (dupTokenAttack (\_ i -> i + 1) attacker) def skelIn
         in skelExpected @=? skelOut
    ]
