module Mahjong.Riichi.FourPlayer where

-- | This module contains the four player riichi variant of mahjong.

import Mahjong.Riichi.Base

data RiichiMahjong
  = RiichiMahjong
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
makeLenses ''RiichiMahjong
