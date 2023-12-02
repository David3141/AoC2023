{-# LANGUAGE TupleSections #-}

module Day02 (part1, part2) where

import Helpers (fMatch, parseInt, readStrings)
import Text.Regex.Applicative
  ( anySym,
    few,
    many,
    string,
    sym,
    (<|>),
  )

data Color
  = Red
  | Green
  | Blue
  deriving (Eq, Ord, Show)

type ColorCount = (Color, Int)

data Game = Game {gameId :: Int, rounds :: [[ColorCount]]}
  deriving (Show)

part1 :: IO Int
part1 = sum . map gameId . filter isGamePossible <$> parseGames
  where
    isGamePossible :: Game -> Bool
    isGamePossible = all (all colorIsValid) . rounds

    colorIsValid (Red, x) = x <= 12
    colorIsValid (Green, x) = x <= 13
    colorIsValid (Blue, x) = x <= 14

part2 :: IO Int
part2 = sum . map powerOfCubes <$> parseGames
  where
    powerOfCubes :: Game -> Int
    powerOfCubes game = minimumReds * minimumGreens * minimumBlues
      where
        minimumReds = getMaxByColor Red game
        minimumGreens = getMaxByColor Green game
        minimumBlues = getMaxByColor Blue game

    isColor :: Color -> ColorCount -> Bool
    isColor color (otherColor, _) = color == otherColor

    getMaxByColor :: Color -> Game -> Int
    getMaxByColor color = snd . maximum . filter (isColor color) . concat . rounds

parseGames :: IO [Game]
parseGames = map parseGame <$> readStrings "inputs/day02.txt"
  where
    parseGame :: String -> Game
    parseGame str =
      Game
        { gameId = fMatch parseGameId str,
          rounds = fMatch parseRounds str
        }
      where
        parseGameId = string "Game " *> parseInt <* many anySym

        parseRounds = few anySym *> many parseRound
        parseRound = many (parseColor <* parseComma) <* many (sym ';')

        parseComma = many $ sym ','
        parseColor = string " " *> (parseRed <|> parseGreen <|> parseBlue)
        parseRed = (Red,) <$> parseInt <* string " red"
        parseGreen = (Green,) <$> parseInt <* string " green"
        parseBlue = (Blue,) <$> parseInt <* string " blue"
