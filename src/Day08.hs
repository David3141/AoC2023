{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day08 (part1, part2) where

import qualified Data.Map.Strict as M
import Helpers (readStrings)

type Network = M.Map String (String, String)

part1 :: IO Int
part1 = stepsUntilCondition (== "ZZZ") "AAA" . parseNodes <$> readStrings "inputs/day08.txt"

part2 :: IO Int
part2 = do
  (directions, network) <- parseNodes <$> readStrings "inputs/day08.txt"

  let endsOnA [_, _, c] = c == 'A'
  let endsOnZ [_, _, c] = c == 'Z'
  let startNodes = M.keys $ M.filterWithKey (\key _ -> endsOnA key) network

  return
    . foldl1 lcm
    . map (\startNode -> stepsUntilCondition endsOnZ startNode (directions, network))
    $ startNodes

parseNodes :: [String] -> (String, Network)
parseNodes (instructions : _ : networkStr) =
  (instructions, M.fromList network)
  where
    network = map parseLine networkStr
    parseLine str = (location, (left, right))
      where
        location = take 3 str
        left = take 3 . drop 7 $ str
        right = take 3 . drop 12 $ str

stepsUntilCondition :: (String -> Bool) -> String -> (String, Network) -> Int
stepsUntilCondition condition startLocation (directions, network) =
  followInstruction initialState
  where
    initialState = (cycle directions, startLocation, 0)

    followInstruction :: (String, String, Int) -> Int
    followInstruction (direction : remainingDirections, location, stepCount)
      | condition location = stepCount
      | otherwise = followInstruction (remainingDirections, newLocation, stepCount + 1)
      where
        (left, right) = network M.! location
        newLocation = if direction == 'L' then left else right
