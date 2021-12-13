module AdventOfCode.Day4 where

import           AdventOfCode.Inputs  (Parser)
import qualified AdventOfCode.Inputs  as Inputs
import           Control.Lens         (_2, folded, over, sumOf, (%~))
import           Data.Foldable        (fold)
import           Data.Function        ((&))
import           Data.List            (scanl', transpose)
import           Data.Monoid          (Any (..))
import qualified Data.Set             as Set
import           Text.Megaparsec      (optional, runParser, some)
import           Text.Megaparsec.Char

newtype Board = Board [[Int]] deriving (Eq, Show)

parseRow :: Parser [Int]
parseRow = do
  hspace
  first <- Inputs.int
  rest <- some (hspace *> Inputs.int)
  _ <- newline
  pure $ first : rest

parseBoard :: Parser Board
parseBoard = do
  _ <- newline
  rows <- some parseRow

  pure $ Board rows

parseDrawNumber :: Parser Int
parseDrawNumber = Inputs.int <* optional (char ',')

game :: Parser ([Int], [Board])
game = do
  draw <- some parseDrawNumber
  _ <- newline
  boards <- some parseBoard

  pure (draw, boards)

winningScore :: ([([Int], [Board])] -> ([Int], Board)) -> [Int] -> [Board] -> Int
winningScore narrow draw boards = last winningDraw * sumOf folded unmarkedNumbers
  where
    (winningDraw, Board rows) = narrow allWinningBoards

    unmarkedNumbers = Set.difference (Set.fromList (fold rows)) (Set.fromList winningDraw)

    winningBoard drawn (Board rows') =
      let drawnSet = Set.fromList drawn
          complete ns = Set.fromList ns `Set.isSubsetOf` drawnSet
       in getAny $ foldMap (Any . complete) rows' <> foldMap (Any . complete) (transpose rows')

    allWinningBoards =
      scanl'
        (\(drawn, winners) n -> (drawn <> [n], winningBoards winners (drawn <> [n])))
        ([], [])
        draw

    winningBoards winners drawn =
      winners <> filter (\board -> board `notElem` winners && winningBoard drawn board) boards

parseGame :: String -> ([Int], [Board])
parseGame input = case runParser game "" input of
  Left _               -> ([], [])
  Right (draw, boards) -> (draw, boards)

part1 :: String -> Int
part1 = uncurry (winningScore firstWinningBoard) . parseGame
  where
    firstWinningBoard = over _2 last . head . dropWhile (\(_, bs) -> null bs)

part2 :: String -> Int
part2 = uncurry (winningScore lastWinningBoard) . parseGame
  where
    lastWinningBoard winningBoards =
      let finalBoards = snd $ last winningBoards
       in dropWhile (\(_, boards) -> boards /= finalBoards) winningBoards
            & head
            & _2
            %~ last
