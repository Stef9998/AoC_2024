module Day2.Utils where

import ReadFile (getInputAsList)

isDecreasing :: (Ord a) => [a] -> Bool
isDecreasing numbers = and $ zipWith (>) numbers (tail numbers)

diffMax3 :: (Num a, Ord a) => [a] -> Bool
diffMax3 numbers = and $ zipWith (\a b -> abs (a - b) <= 3) numbers (tail numbers)

isIncreasing :: (Ord a) => [a] -> Bool
isIncreasing numbers = and $ zipWith (<) numbers (tail numbers)

isSave :: (Num a, Ord a) => [a] -> Bool
isSave numbers = diffMax3 numbers && (isDecreasing numbers || isIncreasing numbers)


-- Parses a single line into a tuple of two Ints
parseLine :: String -> [Int]
parseLine line = map (read :: String -> Int) (words line)

-- Parses the entire file content into two lists
parseFileContent :: [String] -> [[Int]]
parseFileContent = map parseLine


-- Reads and parses the file
readAndParseFile :: String -> IO [[Int]]
readAndParseFile path = do
  content <- getInputAsList path
  return (parseFileContent content)



