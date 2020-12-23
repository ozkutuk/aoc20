{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Arrow ((&&&))
import           Control.Monad.ST.Strict (ST)
import qualified Control.Monad.ST.Strict as ST
import           Data.STRef (STRef)
import qualified Data.STRef as STRef
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List (tails, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T


main :: IO ()
main = do
  adapters <- fmap (read . T.unpack) . T.lines <$> T.readFile "input.txt"
  putStrLn $ "part1: " <> show (part1 adapters)
  putStrLn $ "part2: " <> show (part2 adapters)

part1 :: [Int] -> Int
part1 xs =  uncurry (*) $ ((length . filter (== 1)) &&& (length . filter (== 3))) $ zipWith (-) (tail xs') xs'
  where
    xs' = prepareAdapters xs

part2 :: [Int] -> Int
part2 = countValids

prepareAdapters :: [Int] -> [Int]
prepareAdapters xs = sort (0 : maximum xs + 3 : xs)

countValids :: [Int] -> Int
countValids xs = ST.runST $ do
  memo <- STRef.newSTRef HM.empty
  go memo (prepareAdapters xs)
  where
    go :: forall s. STRef s (HashMap [Int] Int) -> [Int] -> ST s Int
    go _ [] = pure 1
    go _ [_] = pure 1
    go memo l = do
      memo' <- STRef.readSTRef memo
      case HM.lookup l memo' of
        Nothing -> let potentials = filter validStep . take 3 . tail . tails $ l
                   in fmap sum . traverse goAndMemo $ potentials
        Just subtotal -> pure subtotal
      where
        goAndMemo :: [Int] -> ST s Int
        goAndMemo l' = do
          result <- go memo l'
          STRef.modifySTRef memo (HM.insert l' result)
          pure result

        validStep :: [Int] -> Bool
        validStep [] = False
        validStep (h:_) = h - head l <= 3
