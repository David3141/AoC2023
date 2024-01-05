module MatrixHelpers
  ( module Data.Array.IArray,
    Matrix,
    getCol,
    getRow,
    neighborIndices,
    neighbors,
    neighborsWithIndex,
    findIndex,
    fromStrings,
  )
where

import Data.Array.IArray (bounds, (!))
import qualified Data.Array.IArray as A
import Data.List (find)
import Data.Maybe (fromJust)

type Matrix a = A.Array (Int, Int) a

fromStrings :: (Char -> a) -> [String] -> Matrix a
fromStrings parseChar strings =
  A.array
    ((1, 1), (numRows, numCols))
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

(-->) :: Matrix a -> Int -> [a]
matrix --> m = [val | i <- [1 .. maxN], let val = matrix A.! (m, i)]
  where
    (_, (_, maxN)) = A.bounds matrix

getCol :: Int -> Matrix a -> [a]
getCol n matrix = [val | m <- [1 .. maxM], let val = matrix A.! (m, n)]
  where
    (_, (maxM, _)) = A.bounds matrix

getRow :: Int -> Matrix a -> [a]
getRow m matrix = [val | n <- [1 .. maxN], let val = matrix A.! (m, n)]
  where
    (_, (_, maxN)) = A.bounds matrix
