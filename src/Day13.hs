module Day13 (part1, part2) where

import Data.Foldable (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Helpers (countMismatches, readAsSingleString)
import qualified MatrixHelpers as M

type Grid = M.Matrix Char

part1 :: IO Int
part1 = sum . map (patternValue 0) <$> parseGrids

part2 :: IO Int
part2 = sum . map (patternValue 1) <$> parseGrids

patternValue :: Int -> Grid -> Int
patternValue wantedMismatches matrix =
  case verticalMirror of
    Just n -> n
    Nothing -> fromJust horizontalMirror * 100
  where
    verticalMirror = findVerticalMirrorWithMismatch matrix
    horizontalMirror = findVerticalMirrorWithMismatch (M.transpose matrix)

    findVerticalMirrorWithMismatch :: Grid -> Maybe Int
    findVerticalMirrorWithMismatch grid = find isMirrorAt [1 .. (maxN - 1)]
      where
        (_, maxN) = M.maxBounds grid

        isMirrorAt :: Int -> Bool
        isMirrorAt idx = isMirrorWithMismatches backwards forwards
          where
            backwards = map (`M.getCol` grid) [idx, idx - 1 .. 1]
            forwards = map (`M.getCol` grid) [idx + 1 .. maxN]

        isMirrorWithMismatches :: [String] -> [String] -> Bool
        isMirrorWithMismatches as bs = actualMismatches == wantedMismatches
          where
            actualMismatches = sum $ zipWith countMismatches as bs

parseGrids :: IO [Grid]
parseGrids = map parse . splitOn "\n\n" <$> readAsSingleString "inputs/day13.txt"
  where
    parse :: String -> Grid
    parse = M.fromStrings id . lines
