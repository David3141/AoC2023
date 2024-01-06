module MatrixHelpers
  ( module Data.Array.IArray,
    Matrix,
    getCol,
    getRow,
    maxBounds,
    neighborIndices,
    neighbors,
    neighborsWithIndex,
    findIndex,
    fromStrings,
    transpose,
  )
where

import Data.Array.IArray ((!))
import qualified Data.Array.IArray as A
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

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
neighborIndices (m, n) matrix =
  filter (A.inRange $ A.bounds matrix)
    . filter (/= (m, n))
    $ A.range ((m - 1, n - 1), (m + 1, n + 1))

neighbors :: (Int, Int) -> Matrix a -> [a]
neighbors idx matrix =
  map (matrix A.!) (neighborIndices idx matrix)

neighborsWithIndex :: (Int, Int) -> Matrix a -> [((Int, Int), a)]
neighborsWithIndex idx matrix =
  map (\index -> (index, matrix A.! index)) (neighborIndices idx matrix)

getCol :: Int -> Matrix a -> [a]
getCol n matrix = [val | m <- [1 .. maxM], let val = matrix A.! (m, n)]
  where
    (maxM, _) = maxBounds matrix

getRow :: Int -> Matrix a -> [a]
getRow m matrix = [val | n <- [1 .. maxN], let val = matrix A.! (m, n)]
  where
    (_, maxN) = maxBounds matrix

maxBounds :: Matrix a -> (Int, Int)
maxBounds matrix = (maxM, maxN)
  where
    (_, (maxM, maxN)) = A.bounds matrix

transpose :: Matrix a -> Matrix a
transpose matrix =
  A.array
    ((1, 1), swap $ maxBounds matrix)
    [ (swap idx, value)
      | (idx, value) <- A.assocs matrix
    ]
