{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad.Trans
import Network.Wai.Parse
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Aeson (FromJSON, ToJSON)
import Paths_reward_system
import System.Directory
import Web.Scotty

import qualified Reward.Ranking as Ranking

referral :: [String] -> Ranking.Referral
referral (x:y:xs) = Ranking.Referral (read x) (read y)
referral _ = Ranking.Referral 0 0

strToReferrals :: String -> [Ranking.Referral]
strToReferrals str =
  map referral (filter (\xs -> (length xs) == 2) (map words (lines str)))

main :: IO ()
main = do
  ranking <- newMVar Ranking.singleton
  scotty (3000) $ do

    get "/ranking/:uid" $ do
      x <- liftIO $ readMVar ranking
      uid <- param "uid"
      let mdata = Ranking.userMData uid x

      json $ (mdata :: Ranking.MData)

    get "/ranking" $ do
      x <- liftIO $ readMVar ranking
      let r = reverse (sort (Ranking.toList x))
      json $ (r :: [Ranking.MData])

    post "/ranking/file" $ do
      fs <- files
      x <- liftIO $ takeMVar ranking

      let fs' = [ fileContent fi | (fieldName,fi) <- fs ]

      if length fs' == 0 then
        liftIO $ putMVar ranking x
      else
        let referrals = strToReferrals (BS.unpack (head fs'))
            ranking' = x >>= Ranking.insertReferrals referrals
        in liftIO $ putMVar ranking ranking'

      text $ "OK"

    put "/ranking/:src/:dst" $ do
      x <- liftIO $ takeMVar ranking
      src <- param "src"
      dst <- param "dst"

      let ranking' = x >>= Ranking.insertReferral (Ranking.Referral src dst)
      liftIO $ putMVar ranking ranking'

      text $ "OK"
