module AdventOfCode.Day2 where

import           AdventOfCode.Inputs  (Parser)
import qualified AdventOfCode.Inputs  as Inputs
import           Control.Applicative  ((<|>))
import           Data.Either          (fromRight)
import           Data.List            (foldl')
import           Text.Megaparsec      (runParser)
import           Text.Megaparsec.Char

data Command
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show, Eq)

parseCommand :: Parser Command
parseCommand = do
  command <- (Up <$ string "up") <|> (Down <$ string "down") <|> (Forward <$ string "forward")
  space
  count <- Inputs.int

  pure (command count)

{-
    forward X increases the horizontal position by X units.
    down X increases the depth by X units.
    up X decreases the depth by X units.
-}
part1 :: [String] -> Int
part1 rawCommands = uncurry (*) finalPosition
  where
    commands = fromRight [] $ traverse (runParser parseCommand "") rawCommands
    finalPosition = foldl' updatePosition (0, 0) commands
    updatePosition (x, y) c = case c of
      Forward n -> (x + n, y)
      Up n      -> (x, y - n)
      Down n    -> (x, y + n)

{-

    down X increases your aim by X units.
    up X decreases your aim by X units.
    forward X does two things:
        It increases your horizontal position by X units.
        It increases your depth by your aim multiplied by X.
-}

part2 :: [String] -> Int
part2 rawCommands = uncurry (*) finalPosition
  where
    commands = fromRight [] $ traverse (runParser parseCommand "") rawCommands
    finalPosition = (\(a, b, _) -> (a, b)) $ foldl' updatePosition (0, 0, 0) commands
    updatePosition (x, y, z) c = case c of
      Forward n -> (x + n, y + (z * n), z)
      Up n      -> (x, y, z - n)
      Down n    -> (x, y, z + n)
