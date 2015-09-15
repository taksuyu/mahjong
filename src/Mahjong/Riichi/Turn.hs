module Mahjong.Riichi.Turn where

data Turn
  = EastTurn
  | SouthTurn
  | WestTurn
  | NorthTurn
  deriving (Eq, Ord, Enum, Bounded, Show)
