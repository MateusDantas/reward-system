module Reward.RankingTest (
  listOfReferrals
) where

import Test.HUnit
import Data.Maybe

import qualified Reward.Ranking as R

listOfReferrals :: Test
listOfReferrals =
  let r = R.singleton
      refs = [(R.Referral 1 2), (R.Referral 1 3), (R.Referral 2 4)
              , (R.Referral 2 5), (R.Referral 5 6)]
      r' = R.insertReferrals refs (fromJust r)
      expected = [(R.MData 1 1.5 True), (R.MData 2 1.0 True), (R.MData 3 0 False)
                , (R.MData 4 0.0 False), (R.MData 5 0.0 True), (R.MData 6 0.0 False)]
  in TestCase $ do
      assertEqual "list of referrals" expected (R.toList r')


repeatedReferrals :: Test
repeatedReferrals =
  let r = R.singleton
      refs = [(R.Referral 1 2), (R.Referral 1 3) , (R.Referral 3 1)
              , (R.Referral 4 5), (R.Referral 5 1)]
      r' = R.insertReferrals refs (fromJust r)
      expected = [(R.MData 1 1.0 True), (R.MData 2 0.0 False), (R.MData 3 0.0 True)
                , (R.MData 4 1.0 True), (R.MData 5 0.0 False)]
  in TestCase $ do
      assertEqual "repeated referrals" expected (R.toList r')
