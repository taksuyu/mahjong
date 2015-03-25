module Mahsjong.Riichi.Shanten where

import           Mahsjong.Tile

import           Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS

-- | Will find any Tile that occurs at least twice in a MultiSet
pairs :: MultiSet Tile -> MultiSet Tile
pairs a = MS.filter (\b -> 2 <= MS.occur b a) a

pairs' :: MultiSet Tile -> MultiSet Tile
pairs' a = MS.filter (\b -> 2 == MS.occur b a) a

sets :: MultiSet Tile -> MultiSet Tile
sets a = MS.filter (\b -> 3 <= MS.occur b a) a

ends :: MultiSet Tile -> MultiSet Tile
ends = MS.filter end

-- | Handles the worst case scenario for Shanten that with any given
-- hand you will always be within 7 individual tiles from
-- victory. This assumption is validated by the fact you'll always
-- have at least 1 tile of the pairs needed for this win case.
chiitoitsuShanten :: MultiSet Tile -> Int
chiitoitsuShanten a = 7 - MS.distinctSize (pairs a)

-- | Handles the edge case of Shanten that has all 13 end tiles and a
-- pair of one of them. For scoring how this hand came to be is
-- important, but not handled by this function.
orphanShanten :: MultiSet Tile -> Int
orphanShanten a = 14 - dEnds a - dEndPairs a
  where
    dEnds     = MS.distinctSize . ends
    dEndPairs = dEnds . pairs

