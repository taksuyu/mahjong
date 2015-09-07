module Mahjong.Riichi.ThreePlayer where

import Mahjong.Riichi.Base

data RiichiMahjong
  = RiichiMahjong
    { playerEast  :: Player
    , playerSouth :: Player
    , playerWest  :: Player
    , wall        :: Pile
    , round       :: Round
    , hand        :: Turn
    , bonus       :: Integer
    }
  deriving (Show)
makeFields ''RiichiMahjong
