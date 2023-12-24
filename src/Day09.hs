module Day09 (part1, part2) where

import Helpers (readIntLists)

part1 :: IO Int
part1 = sum . map interpolateRight <$> readIntLists " " "inputs/day09.txt"

part2 :: IO Int
part2 = sum . map interpolateLeft <$> readIntLists " " "inputs/day09.txt"

listWithDeltas :: [Int] -> [[Int]]
listWithDeltas = takeWhile (any (/= 0)) . iterate deltas
  where
    deltas :: [Int] -> [Int]
    deltas [] = []
    deltas [_] = []
    deltas (x : y : rest) = (y - x) : deltas (y : rest)

interpolateRight :: [Int] -> Int
interpolateRight xs = foldr (\x acc -> last x + acc) 0 (listWithDeltas xs)

interpolateLeft :: [Int] -> Int
interpolateLeft xs = foldr (\x acc -> head x - acc) 0 (listWithDeltas xs)
