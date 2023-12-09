{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day05 (part1, part2) where

import Data.List.Split (splitOn)
import Helpers (readAsSingleString)

type Seeds = [Int]

type Mapper = (Int, Int, Int)

type MapperGrid = [Mapper]

part1 :: IO Int
part1 = minimum . execAlmanac . parseAlmanac <$> readAsSingleString "inputs/day05.txt"
  where
    execAlmanac :: (Seeds, [MapperGrid]) -> [Int]
    execAlmanac (seeds, mapperGrids) = map (execMapperGrids mapperGrids) seeds

    execMapperGrids :: [MapperGrid] -> Int -> Int
    execMapperGrids [] num = num
    execMapperGrids (mapperGrid : rest) num = execMapperGrids rest $ execMapperGrid mapperGrid num

    execMapper :: Mapper -> Int -> Maybe Int
    execMapper (destination, source, rangeLength) num
      | num >= source && num < source + rangeLength = Just $ destination + num - source
      | otherwise = Nothing

    execMapperGrid :: MapperGrid -> Int -> Int
    execMapperGrid [] num = num
    execMapperGrid (mapper : rest) num = case execMapper mapper num of
      Just x -> x
      Nothing -> execMapperGrid rest num

part2 :: IO Int
part2 = return 20

parseAlmanac :: String -> (Seeds, [MapperGrid])
parseAlmanac str = (seeds, mappers)
  where
    seeds = map read . splitOn " " . drop 7 . head . lines $ str
    mappers = map (map parseMapper . drop 1 . lines) . drop 1 . splitOn "\n\n" $ str

    parseMapper line = (destination, source, rangeLength)
      where
        [destination, source, rangeLength] = map read . splitOn " " $ line
