module Mahsjong.Meld where

import           Mahsjong.Tile

-- | A valid hand generally consists one pair and any combination of four Melds.
--
-- Some exceptions to this are hands like Chiitoitsu (Seven Pairs) or
-- Thirteen Orphans (A Pair of one End, and one of every other End).
data Meld = Run Tile Tile Tile -- ^ Sequential tiles that don't wrap
          | Set Tile           -- ^ Three of a tile
          | Quad Tile          -- ^ Four of a tile
          deriving (Eq, Show)

