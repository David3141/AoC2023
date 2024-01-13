module Day14 (part1, part2) where

import Data.List (sort, sortBy)
import Data.List.Split (condense, oneOf, split)
import Data.Ord (Down (Down), comparing)
import Helpers (iterateWithCycleDetect, readStrings)
import qualified MatrixHelpers as M

data Direction = North | West | South | East

part1 :: IO Int
part1 = calcLoad . tilt North <$> parse

part2 :: IO Int
part2 = calcLoad . iterateWithCycleDetect cycleTilts 1000000000 <$> parse
  where
    cycleTilts :: M.Matrix Char -> M.Matrix Char
    cycleTilts = tilt East . tilt South . tilt West . tilt North

tilt :: Direction -> M.Matrix Char -> M.Matrix Char
tilt North = M.mapCols shiftOsToFront
tilt West = M.mapRows shiftOsToFront
tilt South = M.mapCols shiftOsToBack
tilt East = M.mapRows shiftOsToBack

shiftOsToFront :: String -> String
shiftOsToFront = concatMap (sortBy (comparing Down)) . split (condense $ oneOf "#")

shiftOsToBack :: String -> String
shiftOsToBack = concatMap sort . split (condense $ oneOf "#")

calcLoad :: M.Matrix Char -> Int
calcLoad matrix = sum . map (columnLoad . (`M.getCol` matrix)) $ [1 .. maxN]
  where
    columnLoad :: String -> Int
    columnLoad str = sum $ zipWith (\n char -> if char == 'O' then n else 0) [maxM, maxM - 1 ..] str

    (maxM, maxN) = M.maxBounds matrix

parse :: IO (M.Matrix Char)
parse = M.fromStrings id <$> readStrings "inputs/day14.txt"
