module MatrixHelpers
  ( Matrix,
    neighborIndices,
    neighbors,
    neighborsWithIndex,
    findIndex,
    fromStrings,
  )
where

import qualified Data.Array.IArray as A
import Data.List (find)
import Data.Maybe (fromJust)

type Matrix a = A.Array (Int, Int) a

fromStrings :: (Char -> a) -> [String] -> Matrix a
fromStrings parseChar strings =
  A.array
    ((1, 1), (numCols, numRows))
    [ ((m, n), parseChar char)
      | (m, string) <- zip [1 ..] strings,
        (n, char) <- zip [1 ..] string
    ]
  where
    numCols = length . head $ strings
    numRows = length strings

findIndex :: (Eq a) => a -> Matrix a -> (Int, Int)
findIndex value = fst . fromJust . find ((== value) . snd) . A.assocs

neighborIndices :: (Int, Int) -> Matrix a -> [(Int, Int)]
neighborIndices (x, y) matrix =
  filter (A.inRange $ A.bounds matrix)
    . filter (/= (x, y))
    $ A.range ((x - 1, y - 1), (x + 1, y + 1))

neighbors :: (Int, Int) -> Matrix a -> [a]
neighbors (x, y) matrix =
  map (matrix A.!) (neighborIndices (x, y) matrix)

neighborsWithIndex :: (Int, Int) -> Matrix a -> [((Int, Int), a)]
neighborsWithIndex (x, y) matrix =
  map (\index -> (index, matrix A.! index)) (neighborIndices (x, y) matrix)
