module Tree (
  Node (Node, key),
  Edge (Edge, src, dst),
  Tree (Tree),
  NodeKey,
  empty,
  singleton,
  insertNode,
  updateNode,
  insertEdge,
  findNode
) where

import Data.Foldable
import qualified Data.IntMap.Strict as IntMap

-- The key must be a unique identifier (i.e Each node must have its own key)
type NodeKey = Int

data Node a = Node {
  key :: NodeKey,
  mdata :: a,
  childs :: [NodeKey],
  parent :: NodeKey
} deriving (Show)

data Edge a = Edge {
  src :: Node a,
  dst :: Node a
} deriving (Show)

data Tree a = Empty | Tree {
  nodes :: IntMap.IntMap (Node a)
} deriving (Show)

instance Foldable Tree where
  toList Empty = []
  toList (Tree nodes) = map (\(_, node) -> (mdata node)) (IntMap.toList nodes)

-------------------------------------------------------------------------------
--  Constructor
-------------------------------------------------------------------------------

empty :: Tree a
empty = Empty

singleton :: Node a -> Tree a
singleton node = Tree $ (IntMap.singleton (key node) node)

-------------------------------------------------------------------------------
--  Mutator
-------------------------------------------------------------------------------

insertNode :: Node a -> Tree a -> Tree a
insertNode _ Empty = Empty
insertNode node tree =
  Tree (IntMap.insert (key node) node (nodes tree))

updateNode :: (a -> a) -> NodeKey -> Tree a -> Tree a
updateNode _ _ Empty = Empty
updateNode tr nKey tree =
  Tree (IntMap.update f nKey (nodes tree))
  where f = (\x -> Just (Node (key x) (tr (mdata x)) (childs x) (parent x)))

insertEdge :: Edge a -> Tree a -> Tree a
insertEdge _ Empty = Empty
insertEdge (Edge src dst) tree =
  let tree' = insertNode dst tree
  in Tree (IntMap.update (insertChild (key dst)) (key src) (nodes tree'))

insertChild :: NodeKey -> Node a -> Maybe (Node a)
insertChild nKey node = Just node {childs = nKey:(childs node)}

-------------------------------------------------------------------------------
--  Query
-------------------------------------------------------------------------------
findNode :: NodeKey -> Tree a -> Maybe (Node a)
findNode _ Empty = Nothing
findNode nKey tree = IntMap.lookup nKey (nodes tree)
