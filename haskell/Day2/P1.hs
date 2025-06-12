module Day2.P1 where

import Data.List (sort)
import Day2.Utils (diffMax3, isDecreasing, isIncreasing, isSave, readAndParseFile)


calc :: (Num a, Ord a) => [[a]] -> Int
calc lines = length $ filter id results
  where
    results = map isSave lines

main :: IO ()
main = do
    lines <- readAndParseFile "day2/input.txt"
    -- lines <- readAndParseFile "day2/sample.txt"
    let totalDifference = calc lines
    print totalDifference
