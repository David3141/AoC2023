module Helpers
  ( countMismatches,
    iterateWithCycleDetect,
    fMatch,
    parseInt,
    readAsSingleString,
    readCommaSeparatedInts,
    readIntLists,
    readInts,
    readStrings,
    sortDesc,
    subseqsOfSize,
    uniqPairs,
  )
where

import Data.List (sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Paths_advent_of_code_y2023 (getDataFileName)
import Text.Regex.Applicative (Alternative (many), RE, match, sym)
import Text.Regex.Applicative.Common (decimal)

readAsSingleString :: FilePath -> IO String
readAsSingleString filePath = readFile =<< getDataFileName filePath

readStrings :: FilePath -> IO [String]
readStrings filePath = lines <$> (readFile =<< getDataFileName filePath)

readInts :: FilePath -> IO [Int]
readInts filePath =
  map read . lines
    <$> (readFile =<< getDataFileName filePath)

readIntLists :: String -> FilePath -> IO [[Int]]
readIntLists separator filePath =
  map (map read . splitOn separator) . lines
    <$> (readFile =<< getDataFileName filePath)

readCommaSeparatedInts :: String -> [Int]
readCommaSeparatedInts = map read . splitOn ","

parseInt :: RE Char Int
parseInt = many (sym ' ') *> decimal

-- | like match but forces the result with fromJust
fMatch :: RE Char c -> String -> c
fMatch regEx = fromJust . match regEx

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortBy (comparing Down)

-- | Get unique pairs, e. g., [1, 4, 7] -> [(1, 4), (1,7), (4,7)]
uniqPairs :: (Ord a) => [a] -> [(a, a)]
uniqPairs xs =
  [ (first, second)
    | first <- xs,
      second <- xs,
      second > first
  ]

-- | https://stackoverflow.com/a/21288092
-- | Returns all unique subsquences of size n. Examples:
-- | ```
-- | subseqsOfSize 2 [1..3] => [[1,2],[1,3],[2,3]]
-- | subseqsOfSize 3 [1..4] => [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]
-- | ```
subseqsOfSize :: Int -> [a] -> [[a]]
subseqsOfSize n xs =
  if n > l then [] else subsequencesBySize xs !! (l - n)
  where
    l = length xs

    subsequencesBySize [] = [[[]]]
    subsequencesBySize (y : ys) =
      let next = subsequencesBySize ys
       in zipWith (++) (map (map (y :)) next ++ [[]]) ([] : next)

-- | Examples:
-- | countMismatches "axx" "abc" => 2
-- | countMismatches [1, 2] [1, 2, 3] => 0
countMismatches :: (Eq a) => [a] -> [a] -> Int
countMismatches xs ys = sum . map fromEnum $ zipWith (/=) xs ys

-- Similar to iterate, repeat a function and take the n'th iteration. Stops as soon as a cycle
-- is detected and uses modulo with the cycle length to calculate the n'th element.
iterateWithCycleDetect :: (Ord a) => (a -> a) -> Int -> a -> a
iterateWithCycleDetect f repeatCount element = result
  where
    result = results !! (cycleEnd - 1 - resultIdx) -- list is reversed, therefore reverse index
    resultIdx = (repeatCount - cycleStart) `rem` (cycleEnd - cycleStart) + cycleStart
    (cycleStart, cycleEnd, results) = findCycle (f element) 1 (Map.singleton element 0) [element]

    findCycle element' idx seenElements elements = case Map.lookup element' seenElements of
      Just seenAt -> (seenAt, idx, elements)
      Nothing ->
        findCycle
          nextElement
          (idx + 1)
          (Map.insert element' idx seenElements)
          (element' : elements)
        where
          nextElement = f element'
