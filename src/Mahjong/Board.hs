{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Stability: Experimental
module Mahjong.Board where

import Data.Map

import Mahjong.Class (Cycle (..))
import Mahjong.Player
import Mahjong.Tile

data Board a
  = Board
    { players    :: Map PlayerSeat (Player a)
    , wall       :: Wall a
    , round      :: Round
    , roundHand  :: RoundHand
    , roundBonus :: Integer }
  deriving (Show)

-- | Aside from the number of players and the initial setup of the wall,
-- everything else about the board stays the same.
defaultBoard :: Map PlayerSeat (Player a) -> Wall a -> Board a
defaultBoard playerMap boardWall
  = Board playerMap boardWall minBound minBound 0

newtype Wall a
  = Wall [a]
  deriving (Functor, Monoid, Show)

-- | Mahjong will normally be player over two rounds and in the case of a tie
-- will go into a third round. The north round could be used, but at the time of
-- writing I am unaware of any variant that does without it being a declared
-- option of the game.
newtype Round
  = Round Wind
  deriving (Eq, Ord, Enum, Bounded, Show, Cycle)

-- | Each round goes through each player acting as dealer (east seat)
newtype RoundHand
  = RoundHand Wind
  deriving (Eq, Ord, Enum, Bounded, Show, Cycle)
