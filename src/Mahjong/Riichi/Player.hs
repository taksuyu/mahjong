module Mahjong.Riichi.Player where

import Control.Lens

import Mahjong.Meld
import Mahjong.Tile

type Tile = RawTile
type Pile = [Tile]

newtype Hand
  = Hand { _unHand :: Pile }
  deriving (Show)
makeLenses ''Hand

data Player
  = Player
    { _score       :: Integer
    , _hand        :: Hand
    , _stolenMelds :: [Meld]
    , _discardPile :: Pile
    , _inRiichi    :: Bool
    , _inFuriten   :: Bool
    }
  deriving (Show)
makeLenses ''Player

defaultPlayer :: Player
defaultPlayer = Player 25000 (Hand []) [] [] False False
