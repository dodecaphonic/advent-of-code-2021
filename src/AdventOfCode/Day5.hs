module AdventOfCode.Day5 where

import           AdventOfCode.Inputs  (Parser)
import qualified AdventOfCode.Inputs  as Inputs
import           Control.Applicative  ((<|>))
import           Data.List            (foldl')
import qualified Data.Map.Strict      as Map
import           Prelude              hiding (lines)
import           Text.Megaparsec      (many, runParser)
import           Text.Megaparsec.Char (char, space, string)

newtype Point = Point (Int, Int) deriving (Eq, Show, Ord)

data Line = Line Point Point deriving (Eq, Show)

parsePoint :: Parser Point
parsePoint = do
  x <- Inputs.int
  _ <- char ','
  y <- Inputs.int

  pure $ Point (x, y)

parseLine :: Parser Line
parseLine = Line <$> parsePoint <* string " -> " <*> parsePoint

parseLines :: Parser [Line]
parseLines = many (parseLine <* space)

lines :: String -> [Line]
lines input = case runParser parseLines "" input of
  Left e   -> error (show e)
  Right ls -> ls

pointX :: Point -> Int
pointX (Point (x, _)) = x

pointY :: Point -> Int
pointY (Point (_, y)) = y

interpolatePoints :: Line -> [Point]
interpolatePoints (Line from to) = go []
  where
    diffX = pointX to - pointX from
    diffY = pointY to - pointY from
    increment d
      | d > 0 = 1
      | d < 0 = -1
      | otherwise = 0
    xIncrement = increment diffX
    yIncrement = increment diffY
    go [] = go [from]
    go (p : ps)
      | p == to = p : ps
      | otherwise = go $ Point (pointX p + xIncrement, pointY p + yIncrement) : p : ps

countOverlapping :: (Line -> Bool) -> String -> Int
countOverlapping filterFn = Map.size . Map.filter (> 1) . foldl' overlapping initialCount . lines
  where
    initialCount = Map.empty :: Map.Map Point Int

    overlapping counts line =
      if filterFn line
        then foldr (Map.alter (\value -> ((+ 1) <$> value) <|> Just 1)) counts (interpolatePoints line)
        else counts

isDiagonal :: Line -> Bool
isDiagonal (Line (Point (x1, y1)) (Point (x2, y2))) =
  x1 /= x2 && y1 /= y2

part1 :: String -> Int
part1 = countOverlapping (not . isDiagonal)

part2 :: String -> Int
part2 = countOverlapping (const True)
