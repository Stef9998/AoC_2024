module Day1.P2 where

import Control.Monad(unless)
import Day1.Utils (readAndParseFile)
import Data.List (sort)


calculateSimularity :: [Int] -> Int -> Int
calculateSimularity list value =
    value * length (filter (==value) list)

calculateTotalSimularity :: [Int] -> [Int] -> Int
calculateTotalSimularity firstList secondList =
  let calculateSimularity' = calculateSimularity secondList
      simularities = [calculateSimularity' item | item <- firstList]
  in sum simularities

main :: IO ()
main = do
  (firstList, secondList) <- readAndParseFile "day1.txt"
  let totalSimularity = calculateTotalSimularity firstList secondList
  putStrLn ("Result: " ++ show totalSimularity)
  unless (totalSimularity == 23046913) $
    error $ "Result is incorrect!" ++ " Calculated value: " ++ show totalSimularity ++ " is not " ++ show 23046913

debug :: IO ()
debug = do
    (firstList, secondList) <- readAndParseFile "day1_sample.txt"
    let calculateSimularity' = calculateSimularity secondList
    print $ [calculateSimularity' item | item <- firstList]


