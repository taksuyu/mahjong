module Mahjong.Riichi.Player where

-- | This module is for the @Player@ data structure which is used extensively
-- throughout Riichi variants of mahjong. The logic behind many player related
-- actions exists within @Mahjong.Riichi.Base@ as @Player@ generates some
-- TemplateHaskell that we make use of to provide that functionality.

import Control.Lens

import Mahjong.Meld
import Mahjong.Tile

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
