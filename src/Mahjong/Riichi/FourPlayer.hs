-- | Stability: Experimental
module Mahjong.Riichi.FourPlayer where

import Mahjong.Board
-- import Mahjong.Player
import Mahjong.Riichi
import Mahjong.Tile

data FourPlayer
  = FourPlayer (Board (Dora, RiichiTile))
  deriving (Show)

data FourPlayerOptions
  = FourPlayerOptions
    { allowNotenRiichi :: Bool
      -- ^ Declaring riichi while you don't have a hand that's in tenpai.
    , optionalYakus    :: YakuOptions }
  deriving (Show)

data YakuOptions
  = YakuOptions
    { renhou :: RenhouOption
      -- ^ Optional yaku, that is obtained by calling ron before the dealer's
      -- second turn or before any called tile. Only non-dealers may get this
      -- yaku.
    }
  deriving (Show)

data RenhouOption
  = RenhouMangan
  | RenhouYakuman
  | RenhouYaku Int
    -- ^ The yaku value is cumulative and is generally set to 5 for the hand to
    -- be at least a mangan.
  | Nashi
  deriving (Eq, Show)

data MahjongError
  = AbandonedGame
  deriving (Eq, Show)
