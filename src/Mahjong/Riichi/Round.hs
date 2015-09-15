module Mahjong.Riichi.Round where

data Round
  = EastRound
  | SouthRound
  | WestRound
  | NorthRound
  deriving (Eq, Ord, Enum, Bounded, Show)
