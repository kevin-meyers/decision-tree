{-# LANGUAGE TupleSections #-}

module Utils where

import Data.Function (on)
import Data.List (sortBy)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

import Lib (bestSplit)

type PartitionPair = (MultiSet Int, MultiSet Int)

type Inputs a = ([[a]], [[PartitionPair]])

orderedYs :: (Ord a) => [a] -> [Int] -> ([a], [Int])
orderedYs xs ys =
  foldr (\(x, y) (xs', ys') -> (x : xs', y : ys')) ([], []) (sortTuple xs ys)

sortTuple :: (Ord a) => [a] -> [b] -> [(a, b)]
sortTuple xs ys = sortBy (compare `on` fst) (zip xs ys)

partition :: [Int] -> [PartitionPair]
partition xs = scanr shiftMaps (MultiSet.fromList xs, MultiSet.empty) xs

shiftMaps :: Int -> PartitionPair -> PartitionPair
shiftMaps x (left, right) = (MultiSet.delete x left, MultiSet.insert x right)

partitionsFrom :: (Ord a) => [[a]] -> [Int] -> Inputs a
partitionsFrom xss ys = foldr (createFeatures ys) ([], []) xss

createFeatures :: (Ord a) => [Int] -> [a] -> Inputs a -> Inputs a
createFeatures ys xs (threshs, ps) = (t : threshs, partition inOrderYs : ps)
  where
    (t, inOrderYs) = orderedYs xs ys

-- should call best split only on yss, and do the partitionsFrom above it.
getBestSplit :: [[PartitionPair]] -> (Int, Int)
getBestSplit ys = bestSplit $ zip [0 ..] $ map (zip [0 ..]) ys

childrenData ::
     Int -> [[PartitionPair]] -> ([[PartitionPair]], [[PartitionPair]])
childrenData i = foldr (\y (l, r) -> (take i y : l, drop i y : r)) ([], [])
