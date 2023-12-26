module Day11 (part1, part2) where

import Data.List (transpose)
import Helpers (readStrings, uniqPairs)

part1 :: IO Int
part1 =
  sum . map distance . uniqPairs . galaxyIndices 2
    <$> readStrings "inputs/day11.txt"

part2 :: IO Int
part2 =
  sum . map distance . uniqPairs . galaxyIndices 1000000
    <$> readStrings "inputs/day11.txt"

distance :: ((Int, Int), (Int, Int)) -> Int
distance ((x1, x2), (y1, y2)) = abs (x1 - y1) + abs (x2 - y2)

galaxyIndices :: Int -> [String] -> [(Int, Int)]
galaxyIndices expandBy strings =
  [ (m, n)
    | (m, row) <- zip (expandRowIndices strings) strings,
      (n, char) <- zip (expandColIndices strings) row,
      char == '#'
  ]
  where
    -- Creates a list of indices, taking expansion into account. For example, if
    -- the 2nd row would be expanded by 10, the result would be [1, 2, 12, 13 ...]
    expandRowIndices :: [String] -> [Int]
    expandRowIndices = go 1
      where
        go :: Int -> [String] -> [Int]
        go idx [] = [idx]
        go idx (string : rest)
          | all (== '.') string = idx : go (idx + expandBy) rest
          | otherwise = idx : go (idx + 1) rest

    expandColIndices :: [String] -> [Int]
    expandColIndices = expandRowIndices . transpose
