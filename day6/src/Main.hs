module Main where

import qualified Data.HashSet as HS
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  groups <- T.splitOn "\n\n" <$> T.readFile "input.txt"
  putStrLn $ "part1: " <> show (part1 groups)
  putStrLn $ "part2: " <> show (part2 groups)

part1 :: [Text] -> Int
part1 = sum . fmap (length . HS.fromList . T.unpack . T.concat . T.lines)

part2 :: [Text] -> Int
part2 = sum . fmap (length . foldr1 HS.intersection . fmap (HS.fromList . T.unpack) . T.lines)

