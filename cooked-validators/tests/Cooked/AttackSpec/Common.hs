module Cooked.AttackSpec.Common (tests) where

import Cooked.Attack.Tweak.Common
import Cooked.MockChain
import Cooked.MockChain.Testing
import qualified Cooked.PlutusWrappers as Pl
import Cooked.TestUtils
import Cooked.Tx.Constraints.Optics
import Cooked.Tx.Constraints.Type
import Data.Default
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "building blocks for tweaks"
    [ testGroup "mkSelectTweak" $
        let skel =
              txSkel
                [ paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 123),
                  paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 234),
                  paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 345)
                ]
         in [ testCase "fail if no applicable modifications" $ -- this one is a regression test
                []
                  @=? getTweak
                    ( mkSelectTweak
                        (paysPKWithDatumConstraintsT % valueL)
                        (\_mcst _value -> Nothing)
                        (const True)
                    )
                    def
                    skel,
              testCase "select applied modification by index" $
                [ ( txSkel
                      [ paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 123),
                        paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 234),
                        paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 789)
                      ],
                    [Pl.lovelaceValueOf 345]
                  )
                ]
                  @=? getTweak
                    ( mkSelectTweak
                        (paysPKWithDatumConstraintsT % valueL)
                        ( \_mcst value ->
                            if value `Pl.geq` Pl.lovelaceValueOf 200
                              then Just $ Pl.lovelaceValueOf 789
                              else Nothing
                        )
                        (== 1)
                    )
                    def
                    skel,
              testCase "return unmodified foci in the right order" $
                [ ( txSkel
                      [ paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 789),
                        paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 234),
                        paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 789)
                      ],
                    [ Pl.lovelaceValueOf 123,
                      Pl.lovelaceValueOf 345
                    ]
                  )
                ]
                  @=? getTweak
                    ( mkSelectTweak
                        (paysPKWithDatumConstraintsT % valueL)
                        (\_mcst _value -> Just $ Pl.lovelaceValueOf 789)
                        (`elem` [0, 2])
                    )
                    def
                    skel
            ]
    ]
