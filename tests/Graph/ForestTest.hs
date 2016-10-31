module Graph.ForestTest (
  singleTreeTest,
  multipleTreesTest,
  repeatedEdgesForestTest
) where

import Test.HUnit
import Data.Maybe
import Control.Monad
import qualified Data.IntMap.Strict as IntMap

import qualified Graph.Tree as Tree
import qualified Graph.Forest as Forest

singleTreeTest :: Test
singleTreeTest =
  let g = Forest.singleton
      edges = [(Tree.Edge (Tree.Node 1 0 [] 0) (Tree.Node 2 0 [] 0)),
               (Tree.Edge (Tree.Node 1 0 [] 0) (Tree.Node 3 0 [] 0))]
      f = (\b a -> Forest.insertEdge a b)
      g' = foldM f (fromJust g) edges
  in TestCase $ do
      assertEqual "single tree on forest" 1 (IntMap.size (Forest.trees (fromJust g')))

multipleTreesTest :: Test
multipleTreesTest =
  let g = Forest.singleton
      edges = [(Tree.Edge (Tree.Node 1 0 [] 0) (Tree.Node 2 0 [] 0)),
               (Tree.Edge (Tree.Node 1 0 [] 0) (Tree.Node 3 0 [] 0)),
               (Tree.Edge (Tree.Node 4 0 [] 0) (Tree.Node 5 0 [] 0)),
               (Tree.Edge (Tree.Node 6 0 [] 0) (Tree.Node 7 0 [] 0))]
      f = (\b a -> Forest.insertEdge a b)
      g' = foldM f (fromJust g) edges
  in TestCase $ do
      assertEqual "multiple trees on forest" 3 (IntMap.size (Forest.trees (fromJust g')))

repeatedEdgesForestTest :: Test
repeatedEdgesForestTest =
  let g = Forest.singleton
      edges = [(Tree.Edge (Tree.Node 1 0 [] 0) (Tree.Node 2 0 [] 0)),
               (Tree.Edge (Tree.Node 3 0 [] 0) (Tree.Node 2 0 [] 0)),
               (Tree.Edge (Tree.Node 3 0 [] 0) (Tree.Node 4 0 [] 0)),
               (Tree.Edge (Tree.Node 4 0 [] 0) (Tree.Node 1 0 [] 0))]
      f = (\b a -> Forest.insertEdge a b)
      g' = foldM f (fromJust g) edges
  in TestCase $ do
      assertEqual "repeated edges on forest" 2 (IntMap.size (Forest.trees (fromJust g')))
