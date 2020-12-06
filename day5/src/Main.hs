module Main where

import           Control.Arrow (first)
import           Data.List (sort)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  boardingPasses <- T.lines <$> T.readFile "input.txt"
  putStrLn $ "part1: " <> show (part1 boardingPasses)
  putStrLn $ "part2: " <> show (part2 boardingPasses)

data Direction = Forward | Backward deriving Show

part1 :: [Text] -> Int
part1 = maximum . collectSeatIds 

part2 :: [Text] -> Int
part2 boardingPasses =
  (+1) . fst . head
  $ dropWhile ((== 1) . uncurry subtract)
  $ zip seats (tail seats)
  where
    seats :: [Int]
    seats = sort . collectSeatIds $ boardingPasses

collectSeatIds :: [Text] -> [Int]
collectSeatIds = fmap (seatId . getBothDirections)

seatId :: ([Direction], [Direction]) -> Int
seatId = uncurry (+) . first (* 8) . both findPosition
  
findPosition :: [Direction] -> Int
findPosition dirs = go 0 (2 ^ (length dirs) - 1) dirs
  where
    go :: Int -> Int -> [Direction] -> Int
    go l _ [] = l
    go l u (d:ds) =
      case d of
        Backward -> go (l + diff) u ds
        Forward  -> go l (u - diff) ds
      where
        diff = (u - l + 1) `div` 2

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

splitDimensions :: Text -> (Text, Text)
splitDimensions = T.splitAt 7

parseDirections :: Text -> [Direction]
parseDirections = fmap parseDirection . T.unpack
  where
    parseDirection :: Char -> Direction
    parseDirection = \case
      'B' -> Backward
      'F' -> Forward
      'R' -> Backward
      'L' -> Forward

getBothDirections :: Text -> ([Direction], [Direction])
getBothDirections = both parseDirections . splitDimensions
