-- | Stability: Experimental
module Mahjong.Riichi.ThreePlayer where

import Mahjong.Board
import Mahjong.Riichi
import Mahjong.Tile

data ThreePlayer
  = ThreePlayer (Board (Dora, RiichiTile))
  deriving (Show)
