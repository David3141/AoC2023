module Day12 (part1, part2) where

import Data.List (group)
import Helpers (readStrings)

part1 :: IO Int
part1 = sum . map countPossibilities <$> parse

part2 :: IO Int
part2 = return 20

countPossibilities :: (String, [Int]) -> Int
countPossibilities (string, counts) =
  length . filter (== counts) . map countDmgGroups . traverse replacementsFor $ string
  where
    replacementsFor :: Char -> [Char]
    replacementsFor '?' = ".#"
    replacementsFor c = [c]

    countDmgGroups :: String -> [Int]
    countDmgGroups = map length . filter ((== '#') . head) . group

parse :: IO [(String, [Int])]
parse = map parseLine <$> readStrings "inputs/day12.txt"
  where
    parseLine s = case break (== ' ') s of
      (string, _ : rawCounts) -> (string, read ("[" ++ rawCounts ++ "]"))
      _ -> error "Cannot parse line"
