module Helpers (fMatch, parseInt, readCommaSeparatedInts, readInts, readStrings) where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Paths_advent_of_code_y2023
import Text.Regex.Applicative (Alternative (many), RE, match, psym)

readStrings :: FilePath -> IO [String]
readStrings filePath = lines <$> (readFile =<< getDataFileName filePath)

readInts :: FilePath -> IO [Int]
readInts filePath =
  map read . lines
    <$> (readFile =<< getDataFileName filePath)

-- readCommaSeparatedInts :: FilePath -> IO [Int]
-- readCommaSeparatedInts filePath =
--   map read . splitOn "," <$> (readFile =<< getDataFileName filePath)
readCommaSeparatedInts :: String -> [Int]
readCommaSeparatedInts = map read . splitOn ","

parseInt :: RE Char Int
parseInt = read <$> many (psym isDigit)

-- | like match but forces the result with fromJust
fMatch :: RE Char c -> String -> c
fMatch regEx = fromJust . match regEx
