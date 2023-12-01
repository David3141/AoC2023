module Helpers
    ( readInts
    , readCommaSeparatedInts
    )
where

import           Data.List.Split                ( splitOn )

import           Paths_advent_of_code_y2023

readInts :: FilePath -> IO [Int]
readInts filePath =
    map read . lines <$> (readFile =<< getDataFileName filePath)

-- readCommaSeparatedInts :: FilePath -> IO [Int]
-- readCommaSeparatedInts filePath =
--   map read . splitOn "," <$> (readFile =<< getDataFileName filePath)

readCommaSeparatedInts :: String -> [Int]
readCommaSeparatedInts = map read . splitOn ","
