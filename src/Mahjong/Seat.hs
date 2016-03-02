{-# LANGUAGE DeriveGeneric #-}

module Mahjong.Seat where

import GHC.Generics

data Seat
  = EastSeat
  | SouthSeat
  | WestSeat
  | NorthSeat
  deriving (Generic, Eq, Ord, Enum, Bounded, Show)
