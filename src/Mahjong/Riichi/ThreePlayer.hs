{-# LANGUAGE TemplateHaskell #-}

module Mahjong.Riichi.ThreePlayer where

import Control.Lens

import Mahjong.Riichi

data RiichiMahjong
  = RiichiMahjong
    { _playerEast  :: Player
    , _playerSouth :: Player
    , _playerWest  :: Player
    , _wall        :: Pile
    , _round       :: Round
    , _hand        :: Turn
    , _bonus       :: Integer
    }
  deriving (Show)
makeLenses ''RiichiMahjong
