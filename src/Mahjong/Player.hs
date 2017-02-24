{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- | Stability: Experimental
module Mahjong.Player where

import Data.Data

import Mahjong.Class
import Mahjong.Tile

-- | Tiles can only be stolen to make a Meld, and once that Meld is formed using
-- a stolen tile it cannot be changed for the duration of the hand. Since
-- visualization is something to consider, we'll support which tile should be
-- represented by their position to the player that stole the tile. For instance
-- if the player that steals is North and the player that is stolen from is
-- South then in a Run the stolen tile will be a second one in the Meld.
-- Functionally this serves no purpose other than a rule of thumb for visual
-- clients.
data Steal a
  = Steal PlayerSeat (Meld a)
  deriving ( Eq, Show
           , Data, Typeable )

instance Functor Steal where
  fmap fn (Steal s a) = Steal s (fmap fn a)

instance Tile a => Tile (Steal a) where
  honor (Steal _ a) = honor a

  terminal (Steal _ a) = terminal a

data Player a
  = Player
    { score       :: Integer
    , hand        :: PlayerHand a
    , stolenMelds :: [Steal a]
    , discardPile :: [a]
    , tenpaiState :: Tenpai }
  deriving ( Show
           , Data, Typeable )

defaultPlayer :: Player a
defaultPlayer = Player 25000 mempty [] [] NotInTenpai

newtype PlayerHand a
  = PlayerHand [a]
  deriving ( Monoid, Show
           , Data, Typeable )

newtype PlayerSeat
  = Seat Wind
  deriving ( Eq, Ord, Enum, Bounded, Show
           , Data, Typeable
           , Cycle )

-- | Player property sum type that allows us to not have to calculate whether
-- they are in tenpai every round. Some special conditions may have to be
-- checked each turn, but a pattern matching case will suffice.
data Tenpai
  = NotInTenpai
    -- ^ Normal state for the hand to be in.
  | InTenpai
    -- ^ One tile away from winning and no special conditions apply.
  | InRiichi
    -- ^ Same as InTenpai, but the player has decided to bet points for an extra
    -- yaku.
  | InFuriten
    -- ^ The player is in a state of tenpai, but cannot win on anything other
    -- than a self draw due to the player discarding a tile that would currently
    -- let them win. This rule can also be found under the name of 'Sacred
    -- Discard'.
  deriving ( Eq, Show
           , Data, Typeable )
