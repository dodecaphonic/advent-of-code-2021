module AdventOfCode.Inputs where

import           Control.Applicative  (many)
import           Control.Lens         (folded, imap, sumOf, (^..))
import           Data.Char            (digitToInt)
import           Data.List            (foldl')
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec)
import           Text.Megaparsec.Char (digitChar)

type Parser = Parsec Void String

digit :: Parser Int
digit = digitToInt <$> digitChar

int :: Parser Int
int = do
  digits <- many digit

  pure . sumOf folded . imap (\i n -> 10 ^ i * n) $ reverse digits

loadInts :: FilePath -> IO [Int]
loadInts = fmap (fmap read) . loadStrings

loadStrings :: FilePath -> IO [String]
loadStrings path = lines <$> readFile path

solve :: (FilePath -> IO [a]) -> ([a] -> b) -> FilePath -> IO b
solve load fn path = fn <$> load path
