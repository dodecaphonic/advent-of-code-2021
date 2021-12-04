module AdventOfCode.Day1 where

import           Data.Foldable (foldMap)
import           Data.Monoid   (Sum (..))

part1 :: [Int] -> Int
part1 is = getSum $ foldMap (\(a, b) -> if b > a then Sum 1 else Sum 0) $ zip is (tail is)

part2 :: [Int] -> Int
part2 is = part1 $ zipWith3 (\a b c -> a + b + c) is (tail is) (tail $ tail is)
