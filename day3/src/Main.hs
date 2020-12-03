{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.Foldable (foldl')
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  levels <- T.lines <$> T.readFile "input.txt"
  putStrLn $ "part1: " <> show (part1 levels)
  putStrLn $ "part2: " <> show (part2 levels)

data Slope = Slope
  { x :: Int
  , y :: Int
  }

part1 :: [Text] -> Int
part1 = treesOnSlope (Slope 3 1)

part2 :: [Text] -> Int
part2 levels = product . fmap (flip treesOnSlope levels) $ slopes
  where
    slopes :: [Slope]
    slopes =
      [ Slope 1 1
      , Slope 3 1
      , Slope 5 1
      , Slope 7 1
      , Slope 1 2
      ]

treesOnSlope :: Slope -> [Text] -> Int
treesOnSlope slope = length . filter (== '#') . steps slope

steps :: Slope -> [Text] -> [Char]
steps Slope{ x, y } = foldl' collectEveryXth [] . zip [0 ..] . fmap (cycle . T.unpack)
  where
    collectEveryXth :: [Char] -> (Int, String) -> [Char]
    collectEveryXth chars (i, line) | i `rem` y == 0 = (line !! (x * (i `div` y))) : chars
                                    | otherwise      = chars
