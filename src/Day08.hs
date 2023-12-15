{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day08 (part1, part2) where

import qualified Data.Map.Strict as Map
import Helpers (readStrings)

type Network = Map.Map String (String, String)

type State = (String, String, Network, Int) -- directions, location, network, step count

part1 :: IO Int
part1 =
  followInstruction . initializeState . parseNodes <$> readStrings "inputs/day08.txt"

part2 :: IO Int
part2 = return 20

parseNodes :: [String] -> (String, Network)
parseNodes (instructions : _ : networkStr) =
  (instructions, Map.fromList network)
  where
    network = map parseLine networkStr
    parseLine str = (location, (left, right))
      where
        location = take 3 str
        left = take 3 . drop 7 $ str
        right = take 3 . drop 12 $ str

initializeState :: (String, Network) -> State
initializeState (directions, network) =
  ( cycle directions,
    "AAA",
    network,
    0
  )

followInstruction :: State -> Int
followInstruction (direction : remainingDirections, location, network, stepCount)
  | location == "ZZZ" = stepCount
  | otherwise = followInstruction (remainingDirections, newLocation, network, stepCount + 1)
  where
    (left, right) = network Map.! location
    newLocation = case direction of
      'L' -> left
      _ -> right
