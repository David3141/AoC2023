module Main (main) where

-- import Control.Monad (mapM_)
import qualified Day01
import qualified Day02
import           System.Environment (getArgs)
import           System.TimeIt (timeIt)

main :: IO ()
main = do
  daysToRun <- map read <$> getArgs
  mapM_ run daysToRun

run :: Int -> IO ()
run 1 = runPretty 1 (Day01.part1, Day01.part2)
run 2 = runPretty 2 (Day02.part1, Day02.part2)
run _ = putStrLn "Not implemented"

-- run 3 = runPretty 3 (Day03.part1, Day03.part2)
-- run 4 = runPretty 4 (Day04.part1, Day04.part2)
-- run 5 = runPretty 5 (Day05.part1, Day05.part2)
-- run 6 = runPretty 6 (Day06.part1, Day06.part2)
-- run 7 = runPretty 7 (Day07.part1, Day07.part2)
-- run  8 = runPretty 8 (Day08.part1, Day08.part2)
-- run  9 = runPretty 9 (Day09.part1, Day09.part2)
-- run 10 = runPretty 10 (Day10.part1, Day10.part2)
-- run 11 = runPretty 11 (Day11.part1, Day11.part2)
-- run 12 = runPretty 12 (Day12.part1, Day12.part2)
-- run 13 = runPretty 13 (Day13.part1, Day13.part2)
-- run 14 = runPretty 14 (Day14.part1, Day14.part2)
-- run 15 = runPretty 15 (Day15.part1, Day15.part2)
-- run 16 = runPretty 16 (Day16.part1, Day16.part2)
-- run 17 = runPretty 17 (Day17.part1, Day17.part2)
runPretty :: (Show a, Show b) => Int -> (IO a, IO b) -> IO ()
runPretty day (part1, part2) = do
  putStrLn $ "--- Day " ++ show day ++ " ---"
  putStr "Part 1:     "
  timeIt (print =<< part1)
  putStr "\nPart 2:     "
  timeIt (print =<< part2)
  putStrLn ""
