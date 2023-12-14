module Helpers
  ( fMatch,
    parseInt,
    readAsSingleString,
    readCommaSeparatedInts,
    readInts,
    readStrings,
    sortDesc,
  )
where

import Data.Char (isDigit)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Paths_advent_of_code_y2023
import Text.Regex.Applicative (Alternative (many), RE, match, psym, sym)

readAsSingleString :: FilePath -> IO String
readAsSingleString filePath = readFile =<< getDataFileName filePath

readStrings :: FilePath -> IO [String]
readStrings filePath = lines <$> (readFile =<< getDataFileName filePath)

readInts :: FilePath -> IO [Int]
readInts filePath =
  map read . lines
    <$> (readFile =<< getDataFileName filePath)

readCommaSeparatedInts :: String -> [Int]
readCommaSeparatedInts = map read . splitOn ","

parseInt :: RE Char Int
parseInt = read <$> (many (sym ' ') *> many (psym isDigit))

-- | like match but forces the result with fromJust
fMatch :: RE Char c -> String -> c
fMatch regEx = fromJust . match regEx

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortBy (comparing Down)
