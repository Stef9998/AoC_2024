module Day1.P1 where

import Day1.Utils (readAndParseFile)
import Data.List (sort)


-- Computes absolute differences between corresponding elements
computeDifferences :: [Int] -> [Int] -> [Int]
computeDifferences = zipWith (\a b -> abs (a - b))

-- Computes total difference
calculateTotalDifference :: [Int] -> [Int] -> Int
calculateTotalDifference firstList secondList =
  let (sortedFirst, sortedSecond) = (sort firstList, sort secondList)
      differences = computeDifferences sortedFirst sortedSecond
   in sum differences

main :: IO ()
main = do
  (firstList, secondList) <- readAndParseFile "day1/input.txt"
  let totalDifference = calculateTotalDifference firstList secondList
  print totalDifference


