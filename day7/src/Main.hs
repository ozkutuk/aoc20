{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Lens (_head, index, toListOf, view)
import qualified Control.Lens.Regex.Text as R
import           Data.Foldable (foldl')
import           Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromJust)
import           Data.Monoid (Sum(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T


main :: IO ()
main = do
  rules <- T.lines <$> T.readFile "input.txt"
  putStrLn $ "part1: " <> show (part1 rules)
  putStrLn $ "part2: " <> show (part2 rules)

data BagContents = BagContents Text [Text] deriving Show

data CountedBagContents = CountedBagContents Text [(Text, Int)] deriving Show

part1 :: [Text] -> Int
part1 rules = (subtract 1) . length . (graph `reachableBy`) . fromJust . getVertex $ "shiny gold"
  where
    (graph, getVertex) = constructGraph . fmap parseRule $ rules

part2 :: [Text] -> Int
part2 = countContainedBags "shiny gold" . createCountedMap . fmap parseRuleWithCount

parseRule :: Text -> BagContents
parseRule rule = BagContents outer inners
  where
    (outer:inners) = view ([R.regex|\b(\w+ \w+) bag|] . R.groups) $ rule

parseRuleWithCount :: Text -> CountedBagContents
parseRuleWithCount rule = CountedBagContents outer innerWithCounts
  where
    outer = view ([R.regex|\b(\w+ \w+) bag|] . index 0 . R.groups . _head) $ rule
    innerWithCounts =
      fmap (\[x,y] -> (y, read . T.unpack $ x)) .
      toListOf ([R.regex|\b(\d+) (\w+ \w+) bag|] . R.groups) $
      rule

createCountedMap :: [CountedBagContents] -> HashMap Text [(Text, Int)]
createCountedMap = foldl' insertBag HM.empty
  where
    insertBag :: HashMap Text [(Text, Int)] -> CountedBagContents -> HashMap Text [(Text, Int)]
    insertBag m (bagToKV -> (bag, inners)) = HM.insert bag inners m

    bagToKV :: CountedBagContents -> (Text, [(Text, Int)])
    bagToKV (CountedBagContents outer inners) = (outer, inners)

countContainedBags :: Text -> HashMap Text [(Text, Int)] -> Int
countContainedBags bag bagCounts = getSum . foldMap (Sum . countForEachBag) $ bagsThisLevel
  where
    countForEachBag :: (Text, Int) -> Int
    countForEachBag (bag', count) = count * (1 + countContainedBags bag' bagCounts)

    bagsThisLevel :: [(Text, Int)]
    bagsThisLevel = bagCounts HM.! bag

constructGraph :: [BagContents] -> (Graph, Text -> Maybe Vertex)
constructGraph bags = (\(x,_,z) -> (x,z)) (G.graphFromEdges graph)
  where
    graph :: [(Int, Text, [Text])]
    graph = fmap (\(BagContents outer inners) -> (1, outer, inners)) bags

reachableBy :: Graph -> Vertex -> [Vertex]
reachableBy = G.reachable . G.transposeG
