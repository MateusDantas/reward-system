module Graph.Forest (
  Forest (Forest, trees),
  empty,
  singleton,
  insertEdge,
  insertNode,
  updateNode,
  rootUpdate,
  toList,
  hasNode,
  findNode
) where

import Data.Maybe
import qualified Data.IntMap.Strict as IntMap
import qualified Graph.Tree as Tree

data Forest a = Empty | Forest {
  trees :: IntMap.IntMap (Tree.Tree a),
  nodeMap :: IntMap.IntMap Tree.NodeKey
} deriving (Show)

-------------------------------------------------------------------------------
-- | Constructor
-------------------------------------------------------------------------------

empty :: Maybe (Forest a)
empty = Just Empty

singleton :: Maybe (Forest a)
singleton = Just (Forest {trees = IntMap.empty, nodeMap = IntMap.empty})

-------------------------------------------------------------------------------
-- | Mutator
-------------------------------------------------------------------------------

insertEdge :: Tree.Edge a -> Forest a -> Maybe (Forest a)
insertEdge edge Empty =
  insertNode (Tree.src edge) Empty >>= insertEdge edge
insertEdge (Tree.Edge src dst) forest =
  let hasSrc = hasNode src forest
      hasDst = hasNode dst forest
  in case (hasSrc, hasDst) of
    (False, _) -> insertNode src forest >>= insertEdge (Tree.Edge src dst)
    (True, False) -> insertEdge' (Tree.Edge src dst) forest
    (True, True) -> Just forest

insertEdge' :: Tree.Edge a -> Forest a -> Maybe (Forest a)
insertEdge' (Tree.Edge src dst) Empty =
  insertNode src Empty >>= insertEdge' (Tree.Edge src dst)
insertEdge' (Tree.Edge src dst) forest =
  let rootKey = fromMaybe 0 (findRootKey (Tree.key src) forest)
      nodeMap' = IntMap.insert (Tree.key dst) rootKey (nodeMap forest)
      f = (\x -> Tree.insertEdge (Tree.Edge src dst) x)
  in Just (Forest (IntMap.adjust f rootKey (trees forest)) nodeMap')

insertNode :: Tree.Node a -> Forest a -> Maybe (Forest a)
insertNode node Empty = singleton >>= insertNode node
insertNode node forest =
  let nodeKey = Tree.key node
      trees' = IntMap.insert nodeKey (Tree.singleton node) (trees forest)
      nodeMap' = IntMap.insert nodeKey nodeKey (nodeMap forest)
  in Just (Forest trees' nodeMap')

updateNode :: (a -> a) -> Tree.Node a -> Forest a -> Maybe (Forest a)
updateNode _ _ Empty = Nothing
updateNode transform node forest =
  let nodeKey = Tree.key node
      rootKey = fromMaybe 0 (findRootKey nodeKey forest)
      f = (\x -> Tree.updateNode transform nodeKey x)
  in Just (Forest (IntMap.adjust f rootKey (trees forest)) (nodeMap forest))

rootUpdate :: (Tree.Tree a -> Tree.Node a -> Tree.Node a) -> Tree.NodeKey -> Forest a -> Maybe (Forest a)
rootUpdate _ _ Empty = Nothing
rootUpdate f nodeKey forest =
  let rootKey = fromMaybe 0 (findRootKey nodeKey forest)
      trees' = (trees forest)
      nodeMap' = (nodeMap forest)
      rootF = Tree.rootUpdate f nodeKey
  in Just (Forest (IntMap.adjust rootF rootKey trees') nodeMap')

-------------------------------------------------------------------------------
-- | Query
-------------------------------------------------------------------------------

hasNode :: Tree.Node a -> Forest a -> Bool
hasNode _ Empty = False
hasNode node forest = IntMap.member (Tree.key node) (nodeMap forest)

findNode :: Tree.NodeKey -> Maybe (Forest a) -> Maybe (Tree.Node a)
findNode _ Nothing = Nothing
findNode nodeKey (Just forest) =
  let rootKey = fromMaybe 0 (findRootKey nodeKey forest)
  in Tree.findNode nodeKey (IntMap.lookup rootKey (trees forest))

findRootKey :: Tree.NodeKey -> Forest a -> Maybe (Tree.NodeKey)
findRootKey _ Empty = Nothing
findRootKey nodeKey forest = IntMap.lookup nodeKey (nodeMap forest)

toList :: Forest a -> [a]
toList Empty = []
toList (Forest trees _) =
  concatMap (\(_, x) -> Tree.toList x) $ (IntMap.toList trees)
