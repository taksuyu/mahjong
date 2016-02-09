module Mahjong.Meld where

import Mahjong.Seat

-- | In most winning hands of Mahjong, Melds are used to group tiles making a
-- hand out of 1 Pair and 4 Melds; they also have a very big interaction in
-- calculating a score for a winning hand
data Meld a
    -- Natural Melds
  = Run a a a
  | Set a
  | Quad a

    -- Stealing Melds
  | StolenRun (Steal a) a a
  | StolenSet (Steal a)
  | StolenQuad (Steal a)
  deriving (Show)

-- | Tiles can only be stolen to make a Meld, and once that Meld is formed using
-- a stolen tile it cannot be changed. While the information isn't crucial to
-- the actual gameplay (as there is no score interaction, and it can't be
-- changed so it's never touched again) visualizing the information requires the
-- information to be available.
data Steal a
  = Steal Seat a
  deriving (Show)

-- | A Wait is an unfinished Meld and allows you to reason about what do you
-- need to get closer to Tenpai (one tile from a winning hand).
data Wait a
  = WRun a a
    -- ^ We don't specify the tile we are waiting for so that you don't have to
    -- treat multisided waits differently.
  | Pair a
    -- ^ Pair is wait for a Set, and Set will naturally be wait for a Quad.
  deriving (Show)
