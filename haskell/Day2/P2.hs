module Day2.P2 where

import Day2.Utils (diffMax3, isDecreasing, isIncreasing, isSave, readAndParseFile)
import Data.List (sort)


getWithoutOneValue :: [a] -> [[a]]
getWithoutOneValue numbers = [take i numbers ++ drop (i + 1) numbers | i <- [0 .. length numbers - 1]]

calc :: (Num a, Ord a) => [[a]] -> Int
calc lines = length $ filter id results
    where results = map (any isSave . getWithoutOneValue) lines

main :: IO ()
main = do
  lines <- readAndParseFile "day2/input.txt"
--   lines <- readAndParseFile "day2/sample.txt"
  let totalDifference = calc lines
  print totalDifference


