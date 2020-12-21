module Main where

import           Control.Applicative (liftA2)
import           Control.Arrow ((&&&))
import           Data.List (inits, tails)
import qualified Data.Text as T
import qualified Data.Text.IO as T


main :: IO ()
main = do
  numbers <- fmap (read . T.unpack) . T.lines <$> T.readFile "input.txt"
  let invalidNumber = part1 numbers
  putStrLn $ "part1: " <> show invalidNumber
  putStrLn $ "part2: " <> show (part2 invalidNumber numbers)

part1 :: [Integer] -> Integer
part1 =
  fst .
  head .
  filter (uncurry notElem) .
  uncurry zip .
  (drop 25 &&& (fmap ((\l -> liftA2 (+) l l) . take 25) . tails))

part2 :: Integer -> [Integer] -> Integer
part2 invalid =
  uncurry (+) .
  (minimum &&& maximum) .
  head .
  filter ((== invalid) . sum) .
  foldMap inits .
  tails
