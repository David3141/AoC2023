module Day13 (part1, part2) where

import Data.Foldable (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Helpers (readAsSingleString)
import qualified MatrixHelpers as M

type Grid = M.Matrix Char

part1 :: IO Int
part1 = sum . map patternValue <$> parseGrids

part2 :: IO Int
part2 = return 20

parseGrids :: IO [Grid]
parseGrids = map parse . splitOn "\n\n" <$> readAsSingleString "inputs/day13.txt"
  where
    parse :: String -> Grid
    parse = M.fromStrings id . lines

patternValue :: Grid -> Int
patternValue matrix = case verticalMirror of
  Just n -> n
  Nothing -> fromJust horizontalMirror * 100
  where
    verticalMirror = find isMirror [1 .. (maxN - 1)]
      where
        (_, (_, maxN)) = M.bounds matrix

        isMirror :: Int -> Bool
        isMirror idx = and $ zipWith (==) backwards forwards
          where
            backwards = map (`M.getCol` matrix) [idx, idx - 1 .. 1]
            forwards = map (`M.getCol` matrix) [idx + 1 .. maxN]

    horizontalMirror = find isMirror [1 .. (maxM - 1)]
      where
        (_, (maxM, _)) = M.bounds matrix

        isMirror :: Int -> Bool
        isMirror idx = and $ zipWith (==) backwards forwards
          where
            backwards = map (`M.getRow` matrix) [idx, idx - 1 .. 1]
            forwards = map (`M.getRow` matrix) [idx + 1 .. maxM]
