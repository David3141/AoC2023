{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use :" #-}

module Day05 (part1, part2) where

import Data.List.Split (splitOn)
import Helpers (readAsSingleString)

type Seeds = [Int]

type SeedRange = (Int, Int) -- (start, end)

type Mapper = (Int, Int, Int) -- (offset, sourceStart, sourceEnd)

type MapperGrid = [Mapper]

part1 :: IO Int
part1 = minimum . execAlmanac . parseAlmanac <$> readAsSingleString "inputs/day05.txt"
  where
    execAlmanac :: (Seeds, [MapperGrid]) -> [Int]
    execAlmanac (seeds, mapperGrids) = map (execMapperGrids mapperGrids) seeds

    execMapperGrids :: [MapperGrid] -> Int -> Int
    execMapperGrids [] num = num
    execMapperGrids (mapperGrid : rest) num = execMapperGrids rest $ execMapperGrid mapperGrid num

    execMapperGrid :: MapperGrid -> Int -> Int
    execMapperGrid [] num = num
    execMapperGrid (mapper : rest) num = case execMapper mapper num of
      Just x -> x
      Nothing -> execMapperGrid rest num

    execMapper :: Mapper -> Int -> Maybe Int
    execMapper (offset, start, end) num
      | num >= start && num <= end = Just $ num + offset
      | otherwise = Nothing

part2 :: IO Int
part2 =
  fst . minimum . execRangeAlmanac . seedsToRanges . parseAlmanac
    <$> readAsSingleString "inputs/day05.txt"
  where
    seedsToRanges :: (Seeds, [MapperGrid]) -> ([SeedRange], [MapperGrid])
    seedsToRanges (seeds, mapperGrid) = (toRanges seeds, mapperGrid)

    toRanges :: (Num a) => [a] -> [(a, a)]
    toRanges [] = []
    toRanges [_] = []
    toRanges (start : range : rest) = (start, start + range - 1) : toRanges rest

    execRangeAlmanac :: ([SeedRange], [MapperGrid]) -> [(Int, Int)]
    execRangeAlmanac (seedRanges, mapperGrids) = execMapperGrids mapperGrids seedRanges

    execMapperGrids :: [MapperGrid] -> [SeedRange] -> [SeedRange]
    execMapperGrids [] seedRanges = seedRanges
    execMapperGrids (mapperGrid : remainingMapperGrids) seedRanges =
      execMapperGrids remainingMapperGrids $
        concatMap (execMapperGrid mapperGrid) seedRanges

    execMapperGrid :: MapperGrid -> SeedRange -> [SeedRange]
    execMapperGrid [] seedRange = [seedRange]
    execMapperGrid ((offset, start, end) : remainingMappers) (seedStart, seedEnd)
      | isNoOverlap = execMapperGrid remainingMappers (seedStart, seedEnd)
      | isFullyInside = [(seedStart + offset, seedEnd + offset)]
      | isStartAndEndOverlap = mappedBefore ++ [mappedFullOverlap] ++ mappedAfter
      | isOnlyStartOverlap = mappedBefore ++ [mappedStartOverlap]
      | otherwise = [mappedEndOverlap] ++ mappedAfter -- isOnlyEndOverlap
      where
        isNoOverlap = seedStart > end || seedEnd < start
        isFullyInside = seedStart >= start && seedEnd <= end
        isStartAndEndOverlap = seedStart < start && seedEnd > end
        isOnlyStartOverlap = seedStart < start && seedEnd <= end

        mappedBefore = execMapperGrid remainingMappers (seedStart, start - 1)
        mappedAfter = execMapperGrid remainingMappers (end + 1, seedEnd)
        mappedFullOverlap = (start + offset, end + offset)
        mappedStartOverlap = (start + offset, seedEnd + offset)
        mappedEndOverlap = (seedStart + offset, end + offset)

parseAlmanac :: String -> (Seeds, [MapperGrid])
parseAlmanac str = (seeds, mappers)
  where
    seeds = map read . splitOn " " . drop 7 . head . lines $ str
    mappers = map (map parseMapper . drop 1 . lines) . drop 1 . splitOn "\n\n" $ str

    parseMapper line = (offset, sourceStart, sourceEnd)
      where
        [destination, sourceStart, rangeLength] = map read . splitOn " " $ line
        sourceEnd = sourceStart + rangeLength - 1
        offset = destination - sourceStart
