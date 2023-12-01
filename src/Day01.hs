module Day01 (part1, part2) where

import           Data.Char (digitToInt, isDigit)
import           Data.Functor (($>))
import           Data.Maybe (fromJust)
import           Text.Regex.Applicative (RE, anySym, few, many, match, psym
                                       , string, (<|>))
import           Helpers (readStrings)

part1 :: IO Int
part1 = sum . map (combineFirstWithLastMatch singleDigit)
  <$> readStrings "inputs/day01.txt"

part2 :: IO Int
part2 = sum . map (combineFirstWithLastMatch (singleDigit <|> writtenNumber))
  <$> readStrings "inputs/day01.txt"

combineFirstWithLastMatch :: RE Char Int -> String -> Int
combineFirstWithLastMatch regEx str = read $ show firstNum ++ show lastNum
  where
    firstNum = fromJust $ firstMatch regEx str
    lastNum = fromJust $ lastMatch regEx str

firstMatch :: RE Char a -> String -> Maybe a
firstMatch regEx = match $ few anySym *> regEx <* many anySym

lastMatch :: RE Char a -> String -> Maybe a
lastMatch regEx = match $ many anySym *> regEx <* few anySym

singleDigit :: RE Char Int
singleDigit = digitToInt <$> psym isDigit

writtenNumber :: RE Char Int
writtenNumber = string "one" $> 1
  <|> string "two" $> 2
  <|> string "three" $> 3
  <|> string "four" $> 4
  <|> string "five" $> 5
  <|> string "six" $> 6
  <|> string "seven" $> 7
  <|> string "eight" $> 8
  <|> string "nine" $> 9
