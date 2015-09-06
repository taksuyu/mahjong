module Mahjong.Riichi.FourPlayer where

import Mahjong.Riichi.Base

data RiichiMahjong
  = RiichiMahjong
    { playerEast  :: Player
    , playerSouth :: Player
    , playerWest  :: Player
    , playerNorth :: Player
    , wall        :: Pile
    , round       :: Round
    , hand        :: Turn
    , bonus       :: Integer
    }
  deriving (Show)
makeFields ''RiichiMahjong
