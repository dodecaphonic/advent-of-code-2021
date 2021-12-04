module AdventOfCode.Inputs where

loadInts :: FilePath -> IO [Int]
loadInts = fmap (fmap read) . loadStrings

loadStrings :: FilePath -> IO [String]
loadStrings path = lines <$> readFile path

solve :: (FilePath -> IO [a]) -> ([a] -> b) -> FilePath -> IO b
solve load fn path = fn <$> load path
