{-# LANGUAGE TemplateHaskell #-}

module Mahjong.Riichi.Player where

import           Control.Lens

import           Mahjong.Meld
import           Mahjong.Tile

type Pile = [Tile]

newtype PlayerHand
  = PlayerHand { _unPlayerHand :: Pile }
  deriving (Show)
makeLenses ''PlayerHand

data Player
  = Player
    { _score       :: Integer
    , _hand        :: PlayerHand
    , _stolenMelds :: [Meld]
    , _discardPile :: Pile
    , _inRiichi    :: Bool
    , _inFuriten   :: Bool
    }
  deriving (Show)
makeLenses ''Player

defaultPlayer :: Player
defaultPlayer = Player 25000 (PlayerHand []) [] [] False False
