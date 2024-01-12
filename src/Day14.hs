module Day14 (part1, part2) where

import Data.List (sortBy, transpose)
import Data.List.Split (condense, oneOf, split)
import Data.Ord (Down (Down), comparing)
import Helpers (readStrings)

part1 :: IO Int
part1 = totalLoad <$> parse

part2 :: IO Int
part2 = return 20

totalLoad :: [String] -> Int
totalLoad strings = sum $ map (load . shiftOsToFront) strings
  where
    strLen = length . head $ strings

    load :: String -> Int
    load str = sum $ zipWith (\n char -> if char == 'O' then n else 0) [strLen, strLen - 1 ..] str

shiftOsToFront :: String -> String
shiftOsToFront = concatMap (sortBy (comparing Down)) . split (condense $ oneOf "#")

-- Transpose input so that "north" becomes the start of each list
parse :: IO [String]
parse = transpose <$> readStrings "inputs/day14.txt"
