module Forest where

import Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import qualified Tree as Tree

data Forest a = Empty | Forest {
  trees :: IntMap.IntMap (Tree.Tree a),
  nodeMap :: IntMap.IntMap Tree.NodeKey
} deriving (Show)

instance Foldable Forest where
  toList Empty = []
  toList (Forest trees _) = concatMap (\x -> toList x) $ trees

-------------------------------------------------------------------------------
--  Constructor
-------------------------------------------------------------------------------

empty :: Maybe (Forest a)
empty = Just Empty

singleton :: Maybe (Forest a)
singleton = Just (Forest {trees = IntMap.empty, nodeMap = IntMap.empty})

-------------------------------------------------------------------------------
--  Mutator
-------------------------------------------------------------------------------

insertEdge :: Tree.Edge a -> Forest a -> Maybe (Forest a)
insertEdge edge Empty =
  insertTree (Tree.src edge) Empty >>= insertEdge edge
insertEdge (Tree.Edge src dst) forest =
  let hasSrc = hasNode src forest
      hasDst = hasNode dst forest
  in case (hasSrc, hasDst) of
    (False, _) -> insertTree src forest >>= insertEdge (Tree.Edge src dst)
    (True, False) -> insertEdge' (Tree.Edge src dst) forest
    (True, True) -> Just forest

insertEdge' :: Tree.Edge a -> Forest a -> Maybe (Forest a)
insertEdge' (Tree.Edge src dst) Empty =
  insertTree src Empty >>= insertEdge' (Tree.Edge src dst)
insertEdge' (Tree.Edge src dst) forest =
  let rootKey = Tree.key src
      nodeMap' = IntMap.insert (Tree.key dst) rootKey (nodeMap forest)
      f = (\x -> Just (Tree.insertEdge (Tree.Edge src dst) x))
  in Just (Forest (IntMap.update f rootKey (trees forest)) nodeMap')

insertTree :: Tree.Node a -> Forest a -> Maybe (Forest a)
insertTree node Empty = singleton >>= insertTree node
insertTree node forest =
  let rootKey = Tree.key node
      trees' = IntMap.insert rootKey (Tree.singleton node) (trees forest)
      nodeMap' = IntMap.insert rootKey rootKey (nodeMap forest)
  in Just (Forest trees' nodeMap')

-------------------------------------------------------------------------------
--  Query
-------------------------------------------------------------------------------

hasNode :: Tree.Node a -> Forest a -> Bool
hasNode _ Empty = False
hasNode node forest = IntMap.member (Tree.key node) (nodeMap forest)
