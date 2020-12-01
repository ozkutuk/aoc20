module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  expenses <- fmap (read . T.unpack) . T.lines <$> T.readFile "input.txt"
  putStrLn $ "part1: " <> show (part1 expenses)
  putStrLn $ "part2: " <> show (part2 expenses)

part1 :: [Integer] -> Integer
part1 = uncurry (*) . findPair

part2 :: [Integer] -> Integer
part2 = multiplyTriplet . findTriplet
  where
    multiplyTriplet (x, y, z) = x * y * z

findPair :: [Integer] -> (Integer, Integer)
findPair xs = head [(x1, x2) | x1 <- xs, x2 <- xs, x1 + x2 == 2020]

findTriplet :: [Integer] -> (Integer, Integer, Integer)
findTriplet xs = head [(x1, x2, x3) | x1 <- xs, x2 <- xs, x3 <- xs, x1 + x2 + x3 == 2020]
