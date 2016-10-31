module Main (
  main
) where

import Control.Monad
import System.Exit
import Graph.TreeTest
import Graph.ForestTest
import Reward.RankingTest
import Test.HUnit

runTests :: IO Counts
runTests = runTestTT $ TestList
  [binaryTreeTest, treeAndRootUpdate, sparseTreeRootUpdate, singleTreeTest
   , repeatedEdgesTest, multipleTreesTest, repeatedEdgesForestTest
   , listOfReferrals]

main :: IO ()
main = do
  r <- runTests
  when (failures r > 0) exitFailure
