{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module AdventOfCode.Day3 (part1, part2, testInputs, parseBits, perColumnBitCounts) where

import           AdventOfCode.Inputs  (Parser)
import           Control.Applicative  (many)
import           Control.Lens
import           Data.Char            (digitToInt)
import           Data.Either          (fromRight)
import           Data.List            (group, sort, transpose)
import           Data.Vector          ((!))
import qualified Data.Vector          as V
import           Text.Megaparsec      (runParser)
import           Text.Megaparsec.Char (binDigitChar)

testInputs :: [String]
testInputs =
  [ "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  ]

toDecimal :: [Int] -> Int
toDecimal = sum . imap (\i n -> 2 ^ i * n) . reverse

parseBits :: [String] -> [[Int]]
parseBits = fromRight [] . traverse (runParser binDigits "")
  where
    binDigits :: Parser [Int]
    binDigits = fmap digitToInt <$> many binDigitChar

perColumnBitCounts :: [[Int]] -> [(Int, Int)]
perColumnBitCounts =
  fmap
    ( ( \case
          [zeroes]       -> (length zeroes, 0)
          [zeroes, ones] -> (length zeroes, length ones)
          _              -> (0, 0)
      )
        . group
        . sort
    )
    . transpose

part1 :: [String] -> Int
part1 is = gammaRate * epsilon
  where
    numbers = perColumnBitCounts . parseBits $ is
    gammaRate = toDecimal $ numbers ^.. each . to (\(a, b) -> if a > b then 0 else 1)
    epsilon = toDecimal $ numbers ^.. each . to (\(a, b) -> if a > b then 1 else 0)

part2 :: [String] -> Int
part2 inputs = generatorRating * scrubberRating
  where
    generatorRating = go (selectBit max 1) numbers 0
    scrubberRating = go (selectBit min 0) numbers 0
    numbers = parseBits inputs
    selectBit select whenTied zeroes ones
      | zeroes == ones = whenTied
      | select zeroes ones == zeroes = 0
      | select zeroes ones == ones = 1
      | otherwise = whenTied

    go :: (Int -> Int -> Int) -> [[Int]] -> Int -> Int
    go choose ns col
      | length ns == 1 = toDecimal $ head ns
      | otherwise =
        let bitCounts = V.fromList $ perColumnBitCounts ns
            (zeroes, ones) = bitCounts ! col
            selectedBit = choose zeroes ones
            remaining = filter (\n -> V.fromList n ! col == selectedBit) ns
         in go choose remaining (col + 1)
