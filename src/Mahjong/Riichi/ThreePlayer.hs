{-# LANGUAGE DeriveDataTypeable #-}

-- | Stability: Experimental
module Mahjong.Riichi.ThreePlayer where

import Data.Data

import Mahjong.Board
import Mahjong.Riichi
import Mahjong.Tile

data ThreePlayer
  = ThreePlayer (Board (Dora, RiichiTile))
  deriving ( Show
           , Data, Typeable )
