module Day15 (part1, part2) where

import Data.Char (ord)
import Data.Foldable (Foldable (foldl'))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IMap
import Data.List.Split (splitOn)
import Helpers (fMatch, parseInt, readStrings)
import Text.Regex.Applicative (anySym, many, sym, (<|>))

data Operation = Remove String | Add String Int deriving (Show, Eq)

part1 :: IO Int
part1 = sum . map hash <$> parse

part2 :: IO Int
part2 = focusingPower . runInstructions . map prepare <$> parse

hash :: String -> Int
hash = foldl' (\acc c -> ((acc + ord c) * 17) `rem` 256) 0

parse :: IO [String]
parse = splitOn "," . head <$> readStrings "inputs/day15.txt"

prepare :: String -> Operation
prepare = fMatch (parseRemove <|> parseAdd)
  where
    parseRemove = Remove <$> (many anySym <* sym '-')
    parseAdd = Add <$> many anySym <* sym '=' <*> parseInt

focusingPower :: IntMap [(String, Int)] -> Int
focusingPower = IMap.foldlWithKey' power 0
  where
    power :: Int -> Int -> [(String, Int)] -> Int
    power acc key lenses = acc + powerOfLenses
      where
        powerOfLenses =
          sum $ zipWith (\slot (_, int) -> (key + 1) * slot * int) [1 ..] lenses

runInstructions :: [Operation] -> IntMap [(String, Int)]
runInstructions = foldl' run IMap.empty
  where
    run intMap (Remove label) = IMap.adjust (filter ((/= label) . fst)) (hash label) intMap
    run intMap (Add label int) = IMap.alter insertOrUpdate (hash label) intMap
      where
        insertOrUpdate Nothing = Just [(label, int)]
        insertOrUpdate (Just lenses) = Just (updateLensValue lenses)
          where
            updateLensValue [] = [(label, int)]
            updateLensValue ((label', val) : rest)
              | label == label' = (label, int) : rest
              | otherwise = (label', val) : updateLensValue rest
