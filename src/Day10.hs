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
part1 = do
  matrix <- parseMatrix

  let startingIndex = M.findIndex Start matrix
  let sizeOfLoop = loopSize 0 startingIndex startingIndex matrix

  return $ sizeOfLoop `div` 2

loopSize :: Int -> (Int, Int) -> (Int, Int) -> M.Matrix Point -> Int
loopSize size previousIdx (m, n) matrix
  | isBeginningFromStart = loopSize (size + 1) (m, n) firstConnectingPipeFromStart matrix
  | hasLoopedBackToStart = size
  | otherwise = loopSize (size + 1) (m, n) nextPipe matrix
  where
    point = matrix A.! (m, n)
    hasLoopedBackToStart = point == Start && previousIdx /= (m, n)
    isBeginningFromStart = point == Start && previousIdx == (m, n)

    upIdx = (m - 1, n)
    rightIdx = (m, n + 1)
    downIdx = (m + 1, n)
    leftIdx = (m, n - 1)

    up = matrix A.! upIdx
    right = matrix A.! rightIdx
    down = matrix A.! downIdx

    firstConnectingPipeFromStart
      | previousIdx /= upIdx && up `elem` [Vertical, SouthEast, SouthWest] = upIdx
      | previousIdx /= rightIdx && right `elem` [Horizontal, NorthWest, SouthWest] = rightIdx
      | previousIdx /= downIdx && down `elem` [Vertical, NorthEast, NorthWest] = downIdx
      | otherwise = leftIdx

    nextPipe
      | goUp = upIdx
      | goRight = rightIdx
      | goDown = downIdx
      | otherwise = leftIdx
      where
        goUp =
          previousIdx == downIdx && point == Vertical
            || previousIdx == leftIdx && point == NorthWest
            || previousIdx == rightIdx && point == NorthEast
        goRight =
          previousIdx == leftIdx && point == Horizontal
            || previousIdx == upIdx && point == NorthEast
            || previousIdx == downIdx && point == SouthEast
        goDown =
          previousIdx == upIdx && point == Vertical
            || previousIdx == leftIdx && point == SouthWest
            || previousIdx == rightIdx && point == SouthEast

part2 :: IO Int
part2 = return 20

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
