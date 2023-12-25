module Day11 (part1, part2) where

import Data.List (transpose)
import Helpers (readStrings)

part1 :: IO Int
part1 =
  sum . map distance . allPairs . galaxyIndices . expand
    <$> readStrings "inputs/day11.txt"

part2 :: IO Int
part2 = return 20

distance :: ((Int, Int), (Int, Int)) -> Int
distance ((x1, x2), (y1, y2)) = abs (x1 - y1) + abs (x2 - y2)

expand :: [String] -> [String]
expand = doubleEmptyRows . doubleEmptyColumns
  where
    doubleEmptyRows :: [String] -> [String]
    doubleEmptyRows = concatMap (\row -> if all (== '.') row then [row, row] else [row])

    doubleEmptyColumns :: [String] -> [String]
    doubleEmptyColumns = transpose . doubleEmptyRows . transpose

galaxyIndices :: [String] -> [(Int, Int)]
galaxyIndices strings =
  [ (m, n)
    | (m, row) <- zip [1 ..] strings,
      (n, char) <- zip [1 ..] row,
      char == '#'
  ]

allPairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
allPairs indices =
  [ (firstIndex, secondIndex)
    | (firstIndex, i) <- zip indices [0 :: Int ..],
      (secondIndex, j) <- zip indices [0 ..],
      j > i
  ]
