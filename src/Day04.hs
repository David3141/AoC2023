{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day04 (part1, part2) where

import Data.List.Split (splitOn)
import Helpers (fMatch, parseInt, readStrings)
import Text.Regex.Applicative (Alternative (many), sym)

part1 :: IO Int
part1 = sum . map calcPoints . parseCards <$> readStrings "inputs/day04.txt"
  where
    calcPoints :: ([Int], [Int]) -> Int
    calcPoints (winningCards, actualCards) =
      pointsForMatches . filter (`elem` winningCards) $ actualCards

    pointsForMatches [] = 0
    pointsForMatches matches = 2 ^ (length matches - 1)

part2 :: IO Int
part2 = sumCardCounts . process . initialize . parseCards <$> readStrings "inputs/day04.txt"
  where
    initialize :: [([Int], [Int])] -> [(Int, Int)] -- [(matchCount, cardCount)]
    initialize = map $ (,1) . countMatches

    countMatches :: ([Int], [Int]) -> Int
    countMatches (winningCards, actualCards) = length . filter (`elem` winningCards) $ actualCards

    process :: [(Int, Int)] -> [(Int, Int)]
    process [] = []
    process ((matchCount, cardCount) : rest) = (matchCount, cardCount) : process restWithNewCounts
      where
        (cardsNeedingUpdate, cardsStayingSame) = splitAt matchCount rest
        updatedCards = map (\(x, count) -> (x, count + cardCount)) cardsNeedingUpdate
        restWithNewCounts = updatedCards ++ cardsStayingSame

    sumCardCounts :: [(Int, Int)] -> Int
    sumCardCounts = sum . map snd

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
