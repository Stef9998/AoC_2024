module ReadFile (readInputFile, getInputAsList) where
import System.Directory (getCurrentDirectory)

import System.FilePath ((</>))

readInputFile :: FilePath -> IO String
readInputFile name = readFile (".." </> "input" </> name)


getInputAsList :: FilePath -> IO [String]
getInputAsList name = lines <$> readInputFile name


main :: IO ()
main = do
  content <- readInputFile "day1/sample.txt"
  putStrLn content