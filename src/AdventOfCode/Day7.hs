module AdventOfCode.Day7 where

import qualified AdventOfCode.Inputs as Inputs

fuelConsumption :: (Int -> Int -> Int) -> Int -> [Int] -> Int
fuelConsumption cost ref xs = sum $ cost ref <$> xs

bestFuelConsumption :: (Int -> Int -> Int) -> [Int] -> Int
bestFuelConsumption cost input = fuelConsumption cost bestReference input
  where
    bestReference = snd . last $ takeWhile latestIsBetter ([1 ..] `zip` [2 ..])
    latestIsBetter (a, b) = fuelConsumption cost b input < fuelConsumption cost a input

solve :: (Int -> Int -> Int) -> String -> Int
solve cost = bestFuelConsumption cost . Inputs.parseInts

part1 :: String -> Int
part1 = solve (\ref n -> abs $ ref - n)

part2 :: String -> Int
part2 = solve (\ref n -> sum [1 .. abs $ ref - n])
