module Day1.Utils where

import ReadFile (readInputFile)


-- Parses a single line into a tuple of two Ints
parseLine :: String -> (Int, Int)
parseLine line =
  let [a, b] = words line
   in (read a :: Int, read b :: Int)

-- Parses the entire file content into two lists
parseFileContent :: String -> ([Int], [Int])
parseFileContent content =
  let pairs = map parseLine (lines content)
   in (map fst pairs, map snd pairs)


-- Reads and parses the file
readAndParseFile :: String -> IO ([Int], [Int])
readAndParseFile path = do
  content <- readInputFile path
  return (parseFileContent content)



