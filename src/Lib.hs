module Lib where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.MultiSet as M

type Probabilities = Map Int Double

type Offset = Int

type MinSplitter = (Double, Offset) -- gini_index, offset

type Partition = M.MultiSet Int

type EnumPartition = (Offset, (Partition, Partition))

type Partitions = [EnumPartition]

type Features = [(Int, Partitions)]

probabilitiesFrom :: Partition -> Probabilities
probabilitiesFrom xs = (/ len) . fromIntegral <$> M.toMap xs
  where
    len = fromIntegral $ M.size xs

entropyOf :: Probabilities -> Double
entropyOf = sum . fmap (\x -> -x * log x)

giniIndexOf :: Probabilities -> Double
giniIndexOf = (1 -) . sum . fmap (^ 2)

updateAcc :: MinSplitter -> MinSplitter -> MinSplitter
updateAcc acc@(min_gini, _) comp@(comp_gini, _)
  | comp_gini < min_gini = comp
  | otherwise = acc

totoalGini :: Int -> EnumPartition -> Double
totoalGini len (offset, (leftCounts, rightCounts)) =
  weightedGini offset leftCounts + weightedGini (len - offset) rightCounts

giniList :: Partitions -> [MinSplitter]
giniList xs = map (\p -> (totoalGini len p, fst p)) xs
  where
    len = length xs

minGini :: [MinSplitter] -> MinSplitter
minGini (x:xs) = foldr minTuple x xs
  where
    minTuple p acc
      | fst p < fst acc = p
      | otherwise = acc

weightedGini :: Int -> Partition -> Double
weightedGini offset = (fromIntegral offset *) . giniIndexOf . probabilitiesFrom

bestSplit :: Features -> (Int, Int)
bestSplit =
  dec .
  foldr (\(i, x) acc -> isBetterGini (minGini $ giniList x) i acc) (200.0, 0, 0) -- gini can at most be 1

dec :: (Double, Int, Int) -> (Int, Int)
dec (_, x, y) = (x, y)

isBetterGini :: MinSplitter -> Int -> (Double, Int, Int) -> (Double, Int, Int)
isBetterGini (gini, offset) feat acc@(accGini, _, _)
  | gini < accGini = (gini, feat, offset)
  | otherwise = acc
