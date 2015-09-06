module Mahjong.Meld
       ( Meld (..)
       , StolenTile (..)
       ) where

import Mahjong.Tile

data StolenTile
  = STile RawTile Int
  deriving (Show)

data Meld
  = Run RawTile RawTile RawTile
  | SRun StolenTile RawTile RawTile
  | Set RawTile
  | SSet StolenTile
  | Quad RawTile
  | SQuad StolenTile
  deriving (Show)
