{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Auction.Offchain where

import qualified Auction as A
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints.Type
import Data.Default
import Data.Maybe
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import qualified Ledger as L
import qualified Ledger.Ada as Ada
import qualified Ledger.Index as Pl
import qualified Ledger.Interval as Interval
import qualified Ledger.Tx as Pl
import qualified Ledger.Value as Value
import Optics.Core
import qualified PlutusTx.Numeric as Pl
import qualified PlutusTx.Prelude as Pl
import Test.QuickCheck.Modifiers (NonZero (..))

-- | Make an offer. There are no checks with this transaction. Anyone is allowed
-- to pay the 'auctionValidator' with something they want to sell, using the
-- 'Offer' datum to specify the seller of the auction.
--
-- This transaction returns the 'SpendableOut' of the 'Offer' UTxO it creates.
txOffer :: MonadWalletMockChain m => L.Value -> Integer -> Wallet -> m SpendableOut
txOffer lot minBid sellerWallet = do
  -- oldUtxos <- scriptUtxosSuchThat A.auctionValidator (\_ _ -> True)
  tx <-
    validateTxSkel sellerWallet []
      mempty
        { _txSkelOpts = def {adjustUnbalTx = True},
          _txSkelOuts = [paysScript A.auctionValidator (A.Offer seller minBid) lot]
        }
  outputs <- spOutsFromCardanoTx tx
  -- the transaction created exactly one script output, so the call to head never fail
  -- newUtxo : _ <- scriptUtxosSuchThat A.auctionValidator (\d x -> d Pl.== A.Offer seller minBid && x `Value.geq` lot)
  -- return $ -- Debug.trace (show tx ++ "\n\n" ++ show (Pl.getCardanoTxOutRefs tx) ++ "\n\n" ++ show (Pl.insert tx mempty)) $
  --   fst newUtxo

  -- uncomment below for something that I would expect to be equivalent, but which isn't:
  return $
    -- Debug.trace (show tx) $
    head $ filter (isJust . sBelongsToScript) outputs
  where
    seller = walletPKHash sellerWallet

checkWallet :: Monad m => L.PubKeyHash -> Wallet -> m ()
checkWallet pkh w
  | pkh /= walletPkh = error $ "wrong wallet arg: got " <> show walletPkh <> "; expected " <> show w
  | otherwise = pure ()
  where
    walletPkh = walletPKHash w

-- | Start an auction by setting the bidding deadline. This transaction consumes
-- the provided 'Offer' Utxo and returns a 'NoBids' UTxO to the auction
-- validator. It also mints the thread NFT that ensures the authenticity of the
-- auction from that point on.
txSetDeadline :: MonadWalletMockChain m => SpendableOut -> L.POSIXTime -> Wallet -> m Pl.CardanoTx
txSetDeadline offerUtxo deadline sellerWallet = do
  let lot = offerUtxo ^. spOutValue
      offerOref = offerUtxo ^. spOutTxOutRef
      theNft = A.threadToken offerOref
  (A.Offer seller minBid) <- spOutGetDatum @A.Auction offerUtxo
  checkWallet seller sellerWallet -- TODO is there a better way?
  validateTxSkel sellerWallet [] $
    mempty
      { _txSkelOpts =
          def
            { adjustUnbalTx = True -- ,
            -- unsafeModTx =
            --   [ RawModTxBeforeBalancing (\tx -> Debug.trace (show tx) tx),
            --     RawModTxAfterBalancing (\tx -> Debug.trace (show tx) tx)
            --   ]
            },
        _txSkelMints =
          txSkelMintsFromList
            [ ( Pl.Versioned A.threadTokenPolicy Pl.PlutusV2,
                SomeMintsRedeemer offerOref,
                A.tokenNameFromTxOutRef offerOref,
                NonZero 1
              )
            ],
        _txSkelIns =
          Set.singleton $
            SpendsScript
              A.auctionValidator
              A.SetDeadline
              offerUtxo,
        _txSkelRequiredSigners = Set.singleton seller,
        _txSkelOuts =
          [ paysScript
              A.auctionValidator
              (A.NoBids seller minBid deadline)
              (lot <> theNft)
          ]
      }

previousBidder :: A.AuctionState -> Maybe (Integer, L.PubKeyHash)
previousBidder (A.Bidding _ _ (A.BidderInfo bid bidder)) = Just (bid, bidder)
previousBidder _ = Nothing

-- | Bid a certain amount of Lovelace on the auction with the given 'Offer'
-- UTxO. If there was a previous bidder, they will receive their money back.
txBid :: MonadWalletMockChain m => SpendableOut -> Integer -> Wallet -> m L.CardanoTx
txBid offerUtxo bid bidderWallet =
  let theNft = A.threadToken $ offerUtxo ^. spOutTxOutRef
      bidder = walletPKHash bidderWallet
   in do
        [(utxo, datum)] <-
          scriptUtxosSuchThat
            A.auctionValidator
            (\_ x -> x `Value.geq` theNft)
        -- The call to 'fromJust' can never fail. If there's already a thread token,
        -- we're at least in 'NoBids' state.
        let deadline = fromJust $ A.getBidDeadline datum
            seller = A.getSeller datum
        validateTxSkel bidderWallet []
          mempty
            { _txSkelOpts = def {adjustUnbalTx = True},
              _txSkelIns =
                Set.singleton $
                  SpendsScript
                    A.auctionValidator
                    (A.Bid (A.BidderInfo bid bidder))
                    utxo,
              _txSkelOuts =
                paysScript
                  A.auctionValidator
                  (A.Bidding seller deadline (A.BidderInfo bid bidder))
                  (utxo ^. spOutValue <> Ada.lovelaceValueOf bid) :
                case previousBidder datum of
                  Nothing -> []
                  Just (prevBid, prevBidder) ->
                    [paysPK prevBidder (Ada.lovelaceValueOf prevBid)],
              _txSkelValidityRange = Interval.to (deadline - 1),
              _txSkelRequiredSigners = Set.singleton bidder
            }

-- | Close the auction with the given 'Offer' UTxO. If there were any bids, this
-- will pay the lot to the last bidder and the last bid to the
-- seller. Otherwise, the seller will receive the lot back. This transaction
-- also burns the thread token.
txHammer :: MonadWalletMockChain m => SpendableOut -> Wallet -> m ()
txHammer offerUtxo sellerWallet =
  let offerOref = offerUtxo ^. spOutTxOutRef
      theNft = A.threadToken offerOref
   in do
        utxos <-
          scriptUtxosSuchThat
            A.auctionValidator
            (\_ x -> x `Value.geq` theNft)
        (A.Offer seller _minBid) <- spOutGetDatum @A.Auction offerUtxo
        -- checkWallet seller sellerWallet
        void $
          validateTxSkel sellerWallet [] $
            mempty
              { _txSkelOpts = def {adjustUnbalTx = True}
              }
              <> case utxos of
                [] ->
                  -- There's no thread token, so the auction is still in 'Offer'
                  -- state
                  mempty
                    { _txSkelIns =
                        Set.singleton $
                          SpendsScript A.auctionValidator (A.Hammer offerOref) offerUtxo,
                      _txSkelOuts =
                        [ paysPK
                            seller
                            (offerUtxo ^. spOutValue)
                        ],
                      _txSkelRequiredSigners = Set.singleton seller
                    }
                (utxo, datum) : _ ->
                  -- There is a thread token, so the auction is in 'NoBids' or
                  -- 'Bidding' state, which means that the following pattern
                  -- match can not fail:
                  let Just deadline = A.getBidDeadline datum
                   in mempty
                        { _txSkelValidityRange = Interval.from deadline,
                          _txSkelIns =
                            Set.singleton $
                              SpendsScript
                                A.auctionValidator
                                (A.Hammer offerOref)
                                utxo,
                          _txSkelMints =
                            review
                              mintsListIso
                              [ ( Pl.Versioned A.threadTokenPolicy Pl.PlutusV2,
                                  SomeMintsRedeemer $ offerUtxo ^. spOutTxOutRef,
                                  A.tokenNameFromTxOutRef offerOref,
                                  NonZero (-1)
                                )
                              ],
                          _txSkelOuts =
                            case previousBidder datum of
                              Nothing ->
                                let lot = utxo ^. spOutValue <> Pl.negate theNft
                                 in [paysPK seller lot]
                              Just (lastBid, lastBidder) ->
                                let lot =
                                      utxo ^. spOutValue
                                        <> Pl.negate (Ada.lovelaceValueOf lastBid)
                                        <> Pl.negate theNft
                                 in [ paysPK lastBidder lot,
                                      paysPK seller (Ada.lovelaceValueOf lastBid)
                                    ]
                        }
