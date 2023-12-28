{-# LANGUAGE MultiWayIf #-}

module Day12 (part1, part2) where

import qualified Data.Array.IArray as A
import Data.List (intercalate)
import Helpers (readStrings)

part1 :: IO Int
part1 = sum . map possibilities <$> parse

part2 :: IO Int
part2 = sum . map (possibilities . unfold) <$> parse
  where
    unfold :: (String, [Int]) -> (String, [Int])
    unfold (string, counts) = (unfoldedStrings, unfoldedCounts)
      where
        unfoldedStrings = intercalate "?" . replicate 5 $ string
        unfoldedCounts = concat . replicate 5 $ counts

{- https://www.youtube.com/watch?v=ltrI20JUAdU -}
possibilities :: (String, [Int]) -> Int
possibilities (string, counts) = subproblems A.! (0, 0)
  where
    subproblems :: A.Array (Int, Int) Int -- (string index, count index) solutions
    subproblems =
      A.array
        ((0, 0), (maxM, maxN))
        [ ((stringIdx, countIdx), subproblem stringIdx countIdx)
          | stringIdx <- [0 .. maxM],
            countIdx <- [0 .. maxN]
        ]

    maxM = length string
    maxN = length counts

    subproblem :: Int -> Int -> Int
    subproblem stringIdx countIdx = case (drop stringIdx string, drop countIdx counts) of
      ([], []) -> 1
      ([], _) -> 0
      (char : chars, ints) -> case char of
        '.' -> skip
        '#' -> use
        '?' -> skip + use
        c -> error $ "Unexpected char: " <> show c
        where
          skip = subproblems A.! (stringIdx + 1, countIdx)
          use = case ints of
            [] -> 0
            (i : remainingIs) ->
              let expected = replicate (i - 1) '#' <> "."
                  expectedAtEnd = replicate (i - 1) '#' -- when string ends on '#'
                  prefix = take i chars

                  match :: Char -> Char -> Char
                  match expectedChar '?' = expectedChar
                  match _ actualChar = actualChar

                  actual = zipWith match expected prefix
               in if
                    | actual == expected -> subproblems A.! (stringIdx + i + 1, countIdx + 1)
                    | actual == expectedAtEnd && null remainingIs -> 1
                    | otherwise -> 0

-- possibilitiesBruteForced :: (String, [Int]) -> Int
-- possibilitiesBruteForced (string, counts) =
--   length . filter (== counts) . map countDmgGroups . traverse replacementsFor $ string
--   where
--     replacementsFor :: Char -> [Char]
--     replacementsFor '?' = ".#"
--     replacementsFor c = [c]

--     countDmgGroups :: String -> [Int]
--     countDmgGroups = map length . filter ((== '#') . head) . group

parse :: IO [(String, [Int])]
parse = map parseLine <$> readStrings "inputs/day12.txt"
  where
    parseLine s = case break (== ' ') s of
      (string, _ : rawCounts) -> (string, read ("[" ++ rawCounts ++ "]"))
      _ -> error "Cannot parse line"
