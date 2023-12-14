module Day07 (part1, part2) where

import Data.List (group, partition, sort)
import Helpers (readStrings, sortDesc)

{-
This solution works by applying 2 rules:

1) The chars of a hand are converted to chars that match their strength:
    For example, in part 1, "T55J5" would be converted to "A55B5" which
    correctly represents the fact that T < J (because A < B). For part 2, that
    string would be converted to "A5505" because the Joker is the weakest card.

2) The `Hand` data type is sortable, first by strength, then by their chars.
    This allows us to easily rank a list of hands.

With these 2 rules, the only thing left to do is to find a way to get a strength
of a hand that is comparable. We achieve this like this: Count duplicates of a
hand ("T55J5" => [1, 3, 1]), square them (=> [1, 9, 1]) and sum the result
(=> 11).

We square the counts because it makes higher duplicates increasingly stronger,
which exactly matches the requirements of the game.

For part 2, we adjust this a little: Count duplicates, but without jokers
("T55J5" => [1, 3]), add the joker count to the highest duplicate count
(=> [1, 4]), and then square and sum them again (=> [1, 16] => 17).
-}

data Hand = Hand
  { cards :: String,
    bid :: Int,
    strength :: Int
  }
  deriving (Show, Eq)

instance Ord Hand where
  compare (Hand cardsA _ strengthA) (Hand cardsB _ strengthB)
    | strengthA == strengthB = compare cardsA cardsB
    | otherwise = compare strengthA strengthB

part1 :: IO Int
part1 =
  totalWinnings . parseHands toOrderedChar calcStrength1
    <$> readStrings "inputs/day07.txt"

part2 :: IO Int
part2 =
  totalWinnings . parseHands toOrderedChar2 calcStrength2
    <$> readStrings "inputs/day07.txt"

totalWinnings :: [Hand] -> Int
totalWinnings = sum . zipWith (\rank hand -> rank * bid hand) [1 ..] . sort

parseHands :: (Char -> Char) -> (String -> Int) -> [String] -> [Hand]
parseHands convertChar calculateStrength = map parseHand
  where
    parseHand str =
      Hand
        { cards = cardsStr,
          bid = read rawBid,
          strength = calculateStrength cardsStr
        }
      where
        (rawCards, rawBid) = splitAt 5 str
        cardsStr = map convertChar rawCards

calcStrength1 :: String -> Int
calcStrength1 = sum . map (square . length) . group . sort

calcStrength2 :: String -> Int
calcStrength2 str = sum . map square $ dupCountsWithJokerApplied
  where
    (jokers, rest) = partition (== '0') str
    jokerCount = length jokers
    descSortedDupCounts = sortDesc . map length . group . sort $ rest

    addToFirst [] _ = [5] -- jokers only, treat as 5 of a kind
    addToFirst (x : xs) val = x + val : xs

    dupCountsWithJokerApplied = addToFirst descSortedDupCounts jokerCount

toOrderedChar :: Char -> Char
toOrderedChar 'A' = 'E'
toOrderedChar 'K' = 'D'
toOrderedChar 'Q' = 'C'
toOrderedChar 'J' = 'B'
toOrderedChar 'T' = 'A'
toOrderedChar c = c

toOrderedChar2 :: Char -> Char
toOrderedChar2 'J' = '0' -- make Joker the weakest
toOrderedChar2 c = toOrderedChar c

square :: Int -> Int
square = (^ (2 :: Int))
