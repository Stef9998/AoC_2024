module ReadFile (readInputFile) where
import System.Directory (getCurrentDirectory)

import System.FilePath ((</>))

readInputFile :: FilePath -> IO String
readInputFile name = readFile (".." </> "input" </> name)


main :: IO ()
main = do
  content <- readInputFile "day1_sample"
  putStrLn content