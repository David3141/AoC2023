{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day06 (part1, part2) where

import Data.Char (isDigit)
import Helpers (fMatch, parseInt, readStrings)
import Text.Regex.Applicative (anySym, few, many)

part1 :: IO Int
part1 = product . map countWins . parseRaces <$> readStrings "inputs/day06.txt"

part2 :: IO Int
part2 = countWins . parseBigRace <$> readStrings "inputs/day06.txt"

parseRaces :: [String] -> [(Int, Int)]
parseRaces (firstLine : secondLine : _) = zip times records
  where
    times = fMatch (few anySym *> many parseInt) firstLine
    records = fMatch (few anySym *> many parseInt) secondLine

parseBigRace :: [String] -> (Int, Int)
parseBigRace (firstLine : secondLine : _) = (time, record)
  where
    time = read . filter isDigit $ firstLine
    record = read . filter isDigit $ secondLine

distance :: Int -> Int -> Int
distance duration hold = hold * (duration - hold)

countWins :: (Int, Int) -> Int
countWins (time, record) = lastWin - firstWin + 1
  where
    firstWin = firstWinInSeries [1 ..]
    lastWin = firstWinInSeries [time, time - 1 ..]

    firstWinInSeries :: [Int] -> Int
    firstWinInSeries = head . dropWhile (\x -> distance time x <= record)
