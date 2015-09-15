module Mahjong.Meld
       ( Meld (..)
       , StolenTile (..)
       ) where

import Mahjong.Seat
import Mahjong.Tile

data StolenTile
  = STile Tile Seat
  deriving (Show)

data Meld
  = Run Tile Tile Tile
  | SRun StolenTile Tile Tile
  | Set Tile
  | SSet StolenTile
  | Quad Tile
  | SQuad StolenTile
  deriving (Show)
