module Main where

import Lib
import Utils

import qualified Data.MultiSet as MultiSet

main :: IO ()
main = putStrLn "Please"

-- should let ys be anything equatable
data DecisionTree a
  = Leaf Int
  | Node ([a] -> Bool) (DecisionTree a) (DecisionTree a)

instance (Show a) => Show (DecisionTree a) where
  show (Leaf x) = show x
  show (Node f left right) = "\nplease" ++ show left ++ " help  " ++ show right

predict :: (Num a) => [a] -> DecisionTree a -> Int
predict _ (Leaf x) = x
predict xs (Node thresh l r)
  | thresh xs = predict xs r
  | otherwise = predict xs l

train :: [[Double]] -> [Int] -> DecisionTree Double
train xs ys = buildTree $ partitionsFrom xs ys

buildTree :: Inputs Double -> DecisionTree Double
buildTree (xs, ys)
  | MultiSet.size y < 5 = Leaf (MultiSet.findMax y)
  | otherwise =
    Node
      (\x -> (x !! bestFeat) > t)
      (buildTree (xs, left))
      (buildTree (xs, right))
  where
    y = describeY ys
    (bestFeat, bestIndex) = getBestSplit ys
    (left, right) = childrenData bestIndex ys
    t = midPoint bestIndex (xs !! bestFeat)

describeY :: [[PartitionPair]] -> Partition
describeY = snd . head . head

midPoint :: Int -> [Double] -> Double
midPoint i ys = ys !! i
--midPoint i ys = (ys !! i + (ys !! (i - 1))) / 2
