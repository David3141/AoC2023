module Day10 (part1, part2) where

import qualified Data.Array.IArray as A
import Helpers (readStrings)
import qualified MatrixHelpers as M

data Point
  = Start
  | Vertical
  | Horizontal
  | NorthWest
  | NorthEast
  | SouthEast
  | SouthWest
  | Empty
  deriving (Eq, Show)

part1 :: IO Int
part1 = (`div` 2) . length . getLoop <$> parseMatrix

part2 :: IO Int
part2 = countEnclosed . getLoop <$> parseMatrix

getLoop :: M.Matrix Point -> [(Int, Int)]
getLoop matrix = go startIndex startIndex
  where
    startIndex = M.findIndex Start matrix

    go prevIdx currIdx
      | hasLoopedBackToStart = []
      | isBeginningFromStart = currIdx : go currIdx firstConnectingPipeFromStart
      | otherwise = currIdx : go currIdx nextPipeIdx
      where
        point = matrix A.! currIdx
        hasLoopedBackToStart = point == Start && prevIdx /= currIdx
        isBeginningFromStart = point == Start && prevIdx == currIdx

        (m, n) = currIdx

        upIdx = (m - 1, n)
        rightIdx = (m, n + 1)
        downIdx = (m + 1, n)
        leftIdx = (m, n - 1)

        up = matrix A.! upIdx
        right = matrix A.! rightIdx
        down = matrix A.! downIdx

        firstConnectingPipeFromStart
          | prevIdx /= upIdx && up `elem` [Vertical, SouthEast, SouthWest] = upIdx
          | prevIdx /= rightIdx && right `elem` [Horizontal, NorthWest, SouthWest] = rightIdx
          | prevIdx /= downIdx && down `elem` [Vertical, NorthEast, NorthWest] = downIdx
          | otherwise = leftIdx

        nextPipeIdx
          | goUp = upIdx
          | goRight = rightIdx
          | goDown = downIdx
          | otherwise = leftIdx
          where
            goUp =
              prevIdx == downIdx && point == Vertical
                || prevIdx == leftIdx && point == NorthWest
                || prevIdx == rightIdx && point == NorthEast
            goRight =
              prevIdx == leftIdx && point == Horizontal
                || prevIdx == upIdx && point == NorthEast
                || prevIdx == downIdx && point == SouthEast
            goDown =
              prevIdx == upIdx && point == Vertical
                || prevIdx == leftIdx && point == SouthWest
                || prevIdx == rightIdx && point == SouthEast

-- | Pick's theorem + shoelace formula
countEnclosed :: [(Int, Int)] -> Int
countEnclosed path = shoelace path - (length path `div` 2) + 1

shoelace :: [(Int, Int)] -> Int
shoelace path = (`div` 2) . abs . sum $ zipWith determinant path shiftedPath
  where
    determinant (x1, y1) (x2, y2) = (x1 * y2) - (y1 * x2)
    shiftedPath = tail path ++ [head path]

parseMatrix :: IO (M.Matrix Point)
parseMatrix = M.fromStrings parseChar <$> readStrings "inputs/day10.txt"
  where
    parseChar :: Char -> Point
    parseChar 'S' = Start
    parseChar '|' = Vertical
    parseChar '-' = Horizontal
    parseChar 'L' = NorthEast
    parseChar 'J' = NorthWest
    parseChar '7' = SouthWest
    parseChar 'F' = SouthEast
    parseChar _ = Empty
