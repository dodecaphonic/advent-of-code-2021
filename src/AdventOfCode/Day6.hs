module AdventOfCode.Day6 where

import           AdventOfCode.Inputs  (Parser)
import qualified AdventOfCode.Inputs  as Inputs
import           Data.List            (foldl', group, sort)
import           Data.Vector          (Vector, (!), (//))
import qualified Data.Vector          as Vector
import           Text.Megaparsec      (runParser, sepBy)
import           Text.Megaparsec.Char (char)

day0 :: Parser [Int]
day0 = Inputs.int `sepBy` char ','

initialState :: [Int] -> Vector Int
initialState = (zeroed //) . map (\g@(x : _) -> (x, length g)) . group . sort
  where
    zeroed = Vector.fromList [0, 0, 0, 0, 0, 0, 0, 0, 0]

nextDay :: Vector Int -> Vector Int
nextDay state = foldl' updateState state (reverse [0 .. 8])
  where
    updateState state' i =
      state' // case i of
        0 -> [(6, state' ! 6 + state ! 0), (8, state ! 0)]
        _ -> [(i - 1, state ! i)]

simulateDay :: Int -> Vector Int -> Vector Int
simulateDay n state = foldl' (\state' _ -> nextDay state') state [1 .. n]

parseInputs :: String -> Vector Int
parseInputs str = case runParser day0 "" str of
  Left _      -> error "Could not parse str"
  Right value -> initialState value

part1 :: String -> Int
part1 = sum . simulateDay 80 . parseInputs

part2 :: String -> Int
part2 = sum . simulateDay 256 . parseInputs
