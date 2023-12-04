module Day03 (part1, part2) where

import Data.Array (Array, Ix (inRange, range), array, bounds, (!))
import Data.List (nub)
import Helpers (readStrings)
import Text.Read (readMaybe)

data Element = Digit Int | Symbol Char | Empty deriving (Show)

-- matrix notation: (m,n) addresses row m, n'th element, starting on (1,1)
type Grid = Array (Int, Int) Element

part1 :: IO Int
part1 = do
  grid <- parseGrid <$> readStrings "inputs/day03.txt"

  let indicesOfFirstValidDigits = filter (`isFirstDigitOfValidNumber` grid) $ range (bounds grid)

  return $ sum . map (`fullNumberAtIndex` grid) $ indicesOfFirstValidDigits

part2 :: IO Int
part2 = do
  grid <- parseGrid <$> readStrings "inputs/day03.txt"

  return $ sum . map (`gearRatio` grid) $ range (bounds grid)

parseGrid :: [String] -> Grid
parseGrid strings =
  array
    ((1, 1), (numCols, numRows))
    [ ((m, n), parse char)
      | (m, string) <- zip [1 ..] strings,
        (n, char) <- zip [1 ..] string
    ]
  where
    numCols = length . head $ strings
    numRows = length strings

    parse :: Char -> Element
    parse '.' = Empty
    parse c = case readMaybe [c] of
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

neighboringIndices :: (Int, Int) -> Grid -> [(Int, Int)]
neighboringIndices (x, y) grid =
  filter (inRange $ bounds grid)
    . filter (/= (x, y))
    $ range ((x - 1, y - 1), (x + 1, y + 1))

neighboors :: (Int, Int) -> Grid -> [Element]
neighboors (x, y) grid =
  map (grid !) (neighboringIndices (x, y) grid)

isFirstDigitOfValidNumber :: (Int, Int) -> Grid -> Bool
isFirstDigitOfValidNumber (m, n) grid
  | not currentIsDigit = False
  | leftNeighborIsDigit = False
  | otherwise = any hasSymbolNeighbor allIndicesOfNumber
  where
    currentIsDigit = isDigit (grid ! (m, n))
    leftNeighborIsDigit = (n /= 1) && isDigit (grid ! (m, n - 1))
    allIndicesOfNumber = indicesOfNumberAtIndex (m, n) grid
    hasSymbolNeighbor index = any isSymbol $ neighboors index grid

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
    indicesOfDigits = filter (\idx -> isDigit $ grid ! idx) $ neighboringIndices index grid
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
