{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  policies <- fmap parse . T.lines <$> T.readFile "input.txt"
  putStrLn $ "part1: " <> show (part1 policies)
  putStrLn $ "part2: " <> show (part2 policies)

data Policy = Policy
  { lowBound :: Int
  , highBound :: Int
  , char :: Char
  , password :: Text
  }

part1 :: [Policy] -> Int
part1 = length . filter satisfiesOld

part2 :: [Policy] -> Int
part2 = length . filter satisfiesNew

parse :: Text -> Policy
parse s = Policy{ lowBound, highBound, char, password }
  where
    [range, dirtyChar, password] = T.splitOn " " s
    char = T.head dirtyChar
    [lowBound, highBound] = fmap (read . T.unpack) $ T.splitOn "-" range

satisfiesOld :: Policy -> Bool
satisfiesOld Policy{ lowBound, highBound, char, password } =
  let
    withinRange = \n -> n >= lowBound && n <= highBound
  in
    withinRange . T.length . T.filter (== char) $ password

satisfiesNew :: Policy -> Bool
satisfiesNew Policy{ lowBound, highBound, char, password } =
  let
    sameWithIndex = (== char) . T.index password . (subtract 1)
    xor a b = (a || b) && not (a && b)
  in
    xor (sameWithIndex lowBound) (sameWithIndex highBound)
