module Ranking where

import Data.Ord (comparing)

import qualified Forest as Forest
import qualified Tree as Tree

data MData = MData {
  key :: Tree.NodeKey,
  score :: Double,
  invited :: Bool
} deriving (Show)

defaultMData = MData {score = 0.0, invited = False}

data Ranking = Ranking {
  forest :: Maybe (Forest.Forest MData)
} deriving (Show)

data Referral = Referral {
  src :: Tree.NodeKey,
  dst :: Tree.NodeKey
} deriving (Show)

instance Eq MData where
  (MData x _ _) == (MData y _ _) = x == y

instance Ord MData where
  compare = comparing score

-------------------------------------------------------------------------------
-- | Constructor
-------------------------------------------------------------------------------

singleton :: Maybe (Ranking)
singleton = Just (Ranking Forest.singleton)

-------------------------------------------------------------------------------
-- | Mutator
-------------------------------------------------------------------------------

insertReferral :: Referral -> Ranking -> Maybe (Ranking)
insertReferral (Referral src dst) (Ranking forest) =
  let srcNode = Tree.Node src (defaultMData {key = src}) [] 0
      dstNode = Tree.Node dst (defaultMData {key = dst}) [] src
      edge = Tree.Edge srcNode dstNode
      f = (\x -> x {invited = True})
      forest' = forest >>= Forest.insertEdge edge >>= Forest.updateNode f srcNode
  in updateScores src (Ranking forest')

updateScores :: Tree.NodeKey -> Ranking -> Maybe (Ranking)
updateScores nKey ranking =
  Just $ Ranking (forest ranking >>= Forest.rootUpdate scoreF nKey)

-------------------------------------------------------------------------------
-- | Static
-------------------------------------------------------------------------------

scoreF :: Tree.Tree MData -> Tree.Node MData -> Tree.Node MData
scoreF tree (Tree.Node key mdata childs parent) =
  let score' = foldl (+) 0.0 (map (\x -> refScore tree x) childs)
  in (Tree.Node key (mdata {score = score'}) childs parent)

refScore :: Tree.Tree MData -> Tree.NodeKey -> Double
refScore tree nKey =
  let mNode = Tree.findNode nKey (Just tree)
  in case (mNode) of
    Nothing   -> 0.0
    Just node -> calculateScore node

calculateScore :: Tree.Node MData -> Double
calculateScore node = if (invited $ Tree.mdata node)
  then 1.0 + (score (Tree.mdata node)) / 2.0
  else 0.0