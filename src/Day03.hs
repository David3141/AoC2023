module Day03 (part1, part2) where

import Data.Array (Ix (range), bounds, (!))
import Data.List (nub)
import Helpers (readStrings)
import qualified MatrixHelpers as M
import Text.Read (readMaybe)

data Element = Digit Int | Symbol Char | Empty deriving (Show)

-- matrix notation: (m,n) addresses row m, n'th element, starting on (1,1)
type Grid = M.Matrix Element

part1 :: IO Int
part1 = do
  grid <- parseGrid

  let indicesOfFirstValidDigits = filter (`isFirstDigitOfValidNumber` grid) $ range (bounds grid)

  return $ sum . map (`fullNumberAtIndex` grid) $ indicesOfFirstValidDigits

part2 :: IO Int
part2 = do
  grid <- parseGrid

  return $ sum . map (`gearRatio` grid) $ range (bounds grid)

parseGrid :: IO Grid
parseGrid = M.fromStrings parseChar <$> readStrings "inputs/day03.txt"
  where
    parseChar :: Char -> Element
    parseChar '.' = Empty
    parseChar c = case readMaybe [c] of
      Just int -> Digit int
      Nothing -> Symbol c

isDigit :: Element -> Bool
isDigit (Digit _) = True
isDigit _ = False

isSymbol :: Element -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isGear :: Element -> Bool
isGear (Symbol '*') = True
isGear _ = False

isFirstDigitOfValidNumber :: (Int, Int) -> Grid -> Bool
isFirstDigitOfValidNumber (m, n) grid
  | not currentIsDigit = False
  | leftNeighborIsDigit = False
  | otherwise = any hasSymbolNeighbor allIndicesOfNumber
  where
    currentIsDigit = isDigit (grid ! (m, n))
    leftNeighborIsDigit = (n /= 1) && isDigit (grid ! (m, n - 1))
    allIndicesOfNumber = indicesOfNumberAtIndex (m, n) grid
    hasSymbolNeighbor index = any isSymbol $ M.neighbors index grid

indicesOfNumberAtIndex :: (Int, Int) -> Grid -> [(Int, Int)]
indicesOfNumberAtIndex (m, n) grid =
  takeWhile (\index -> isDigit $ grid ! index) [(m, i) | i <- [n .. maxN]]
  where
    (_, (_, maxN)) = bounds grid

fullNumberAtIndex :: (Int, Int) -> Grid -> Int
fullNumberAtIndex (m, n) grid = foldl1 (\x y -> x * 10 + y) digits
  where
    digits = map (toNum . (grid !)) $ indicesOfNumberAtIndex (m, n) grid
    toNum (Digit x) = x
    toNum _ = 0

gearRatio :: (Int, Int) -> Grid -> Int
gearRatio index grid
  | not currentIsGear = 0
  | numberCount /= 2 = 0
  | otherwise = product . map (`fullNumberAtIndex` grid) $ indicesOfNumbers
  where
    currentIsGear = isGear (grid ! index)
    indicesOfDigits = filter (\idx -> isDigit $ grid ! idx) $ M.neighborIndices index grid
    numberCount = length indicesOfNumbers
    indicesOfNumbers = nub . map (`indexOfFirstDigit` grid) $ indicesOfDigits

indexOfFirstDigit :: (Int, Int) -> Grid -> (Int, Int)
indexOfFirstDigit (m, n) grid
  | n == 1 = (m, n)
  | leftNeighborIsDigit = indexOfFirstDigit leftNeighborIndex grid
  | otherwise = (m, n)
  where
    leftNeighborIndex = (m, n - 1)
    leftNeighborIsDigit = isDigit $ grid ! leftNeighborIndex
