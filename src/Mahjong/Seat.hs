module Mahjong.Seat where

data Seat
  = EastSeat
  | SouthSeat
  | WestSeat
  | NorthSeat
  deriving (Eq, Ord, Enum, Bounded, Show)
