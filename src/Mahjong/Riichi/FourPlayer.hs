module Mahjong.Riichi.FourPlayer where

-- | This module contains the four player riichi variant of mahjong.

import Mahjong.Riichi.Base

data FourPlayer
  = FourPlayer
    { _playerEast  :: Player
    , _playerSouth :: Player
    , _playerWest  :: Player
    , _playerNorth :: Player
    , _wall        :: Pile
    , _round       :: Round
    , _hand        :: Turn
    , _bonus       :: Integer
    }
  deriving (Show)
makeLenses ''FourPlayer

data FourPlayerOptions
  = FourPlayerOptions
    { _allowNotenRiichi :: Bool -- ^ Declaring riichi while you don't have a
                                -- hand that's in tenpai.
    , optionalYakus     :: YakuOptions
    }

data YakuOptions
  = YakuOptions
    { _renhou :: RenhouOption -- ^ Optional yaku, that is obtained by calling
                              -- ron before the dealer's second turn or before
                              -- any called tile. Only non-dealers may get this
                              -- yaku.
    }

data RenhouOption
  = RenhouMangan
  | RenhouYakuman
  | RenhouYaku Int -- ^ The yaku value is cumulative and is generally set to 5
                   -- for the hand to be at least a mangan.
  | Nashi
  deriving (Eq, Show)
