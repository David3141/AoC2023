{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day04 (part1, part2) where

import Data.List.Split (splitOn)
import Helpers (fMatch, parseInt, readStrings)
import Text.Regex.Applicative (Alternative (many), sym)

part1 :: IO Int
part1 = sum . map calcPoints . parseCards <$> readStrings "inputs/day04.txt"

part2 :: IO Int
part2 = return 20

calcPoints :: ([Int], [Int]) -> Int
calcPoints (winningCards, actualCards) =
  pointsForMatches . filter (`elem` winningCards) $ actualCards
  where
    pointsForMatches [] = 0
    pointsForMatches matches = 2 ^ (length matches - 1)

parseCards :: [String] -> [([Int], [Int])]
parseCards = map parseCard
  where
    parseCard :: String -> ([Int], [Int])
    parseCard str = (winningCards, actualCards)
      where
        [_, cards] = splitOn ": " str
        [winningString, actualString] = splitOn " | " cards
        whitespace = many $ sym ' '
        winningCards = fMatch (many $ whitespace *> parseInt <* whitespace) winningString
        actualCards = fMatch (many $ whitespace *> parseInt <* whitespace) actualString
