module Mahsjong.Riichi.Meld where

import           Mahsjong.Meld
import           Mahsjong.Tile

-- | Riichi Mahjong allows players to steal a `Tile` to complete a
-- `Meld`; this `Meld` is then considered open and visible to all
-- players with the stolen `Tile` being used as a designator towards
-- the position of the player it was stolen from.
--
-- For example, if a player steals a Character 5 to complete the Run
-- of 4-5-6. The 5 is turned sideways and positioned relative to the
-- player the tile was stolen from.
-- If that player is to the left then it'll be 5-4-6.
-- If that player is to the right then it'll be 4-6-5.
-- If that player is across the table then it'll be 4-5-6.
data RMeld = RMeld { meld :: Meld
                   , open :: Maybe (Player, Tile)
                   } deriving (Eq, Show)
