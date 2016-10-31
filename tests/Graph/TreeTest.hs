module Graph.TreeTest (
  binaryTreeTest,
  treeAndRootUpdate,
  sparseTreeRootUpdate,
  repeatedEdgesTest
) where

import Test.HUnit
import qualified Data.IntMap.Strict as IntMap

import qualified Graph.Tree as Tree

binaryTreeTest :: Test
binaryTreeTest =
  let g = Tree.singleton (Tree.Node 1 6 [] 0)
      edges = [(Tree.Edge (Tree.Node 1 6 [] 0) (Tree.Node 2 5 [] 0)),
               (Tree.Edge (Tree.Node 1 6 [] 0) (Tree.Node 3 4 [] 0)),
               (Tree.Edge (Tree.Node 2 5 [] 0) (Tree.Node 4 3 [] 0)),
               (Tree.Edge (Tree.Node 2 5 [] 0) (Tree.Node 5 2 [] 0))]
      g' = foldr (Tree.insertEdge) g (reverse edges)
  in TestCase $ do
      assertEqual "binary tree" [6, 5, 4, 3, 2] (Tree.toList g')

repeatedEdgesTest :: Test
repeatedEdgesTest =
  let g = Tree.singleton (Tree.Node 1 5 [] 0)
      edges = [(Tree.Edge (Tree.Node 1 6 [] 0) (Tree.Node 2 6 [] 0)),
               (Tree.Edge (Tree.Node 1 6 [] 0) (Tree.Node 2 4 [] 0)),
               (Tree.Edge (Tree.Node 2 5 [] 0) (Tree.Node 4 2 [] 0)),
               (Tree.Edge (Tree.Node 2 5 [] 0) (Tree.Node 4 3 [] 0))]
      g' = foldr (Tree.insertEdge) g (reverse edges)
  in TestCase $ do
      assertEqual "repeated edges tree" [5, 6, 2] (Tree.toList g')
      assertEqual "repeated edges tree size" 3 (IntMap.size (Tree.nodes g'))

treeAndRootUpdate :: Test
treeAndRootUpdate =
  let g = Tree.singleton (Tree.Node 6 6 [] 0)
      edges = [(Tree.Edge (Tree.Node 6 0 [] 0) (Tree.Node 1 5 [] 0)),
               (Tree.Edge (Tree.Node 1 0 [] 0) (Tree.Node 2 4 [] 0)),
               (Tree.Edge (Tree.Node 2 0 [] 0) (Tree.Node 4 3 [] 0)),
               (Tree.Edge (Tree.Node 4 0 [] 0) (Tree.Node 3 2 [] 0))]
      f = (\_ n -> n {Tree.mdata = 1})
      g' = Tree.rootUpdate f 4 (foldr (Tree.insertEdge) g (reverse edges))
  in TestCase $ do
      assertEqual "line tree w/ root update" [1, 1, 2, 1, 1] (Tree.toList g')

sparseTreeRootUpdate :: Test
sparseTreeRootUpdate =
  let g = Tree.singleton (Tree.Node 1 1 [] 0)
      edges = [(Tree.Edge (Tree.Node 1 0 [] 0) (Tree.Node 2 4 [] 0)),
               (Tree.Edge (Tree.Node 1 0 [] 0) (Tree.Node 3 3 [] 0)),
               (Tree.Edge (Tree.Node 2 0 [] 0) (Tree.Node 4 6 [] 0)),
               (Tree.Edge (Tree.Node 3 0 [] 0) (Tree.Node 5 1 [] 0)),
               (Tree.Edge (Tree.Node 3 0 [] 0) (Tree.Node 6 2 [] 0)),
               (Tree.Edge (Tree.Node 5 0 [] 0) (Tree.Node 7 9 [] 0)),
               (Tree.Edge (Tree.Node 2 0 [] 0) (Tree.Node 8 10 [] 0))]
      f = (\_ n -> n {Tree.mdata = (Tree.mdata n) + 1})
      g' = Tree.rootUpdate f 7 (foldr (Tree.insertEdge) g (reverse edges))
    in TestCase $ do
      assertEqual "sparse tree w/ update" [2, 4, 4, 6, 2, 2, 10, 10] (Tree.toList g')
