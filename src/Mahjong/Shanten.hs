module Mahjong.Shanten
       ( Shanten (..)
       , Pair (..)
       , End (unEnd)
       , mkEnd
       , EndPair (unEndPair)
       , mkEndPair
       ) where

import           Mahjong.Meld
import           Mahjong.Tile

-- | Shanten is a way of figuring out how many tiles you need tell you
-- have a hand that is ready. While traditionally this is a number,
-- the structure allows us to buildup a hand in a free form way; this
-- could have a many advantages to this solving space because we can
-- track what parts of the hand are effected with every draw and
-- discard in a build up and break down fashion.
data Shanten
  = Hand Meld Meld Meld Meld Pair
  | Chii Pair Pair Pair Pair Pair Pair Pair
  | TOrph End End End End End End End End End End End End EndPair
  deriving (Show)

data Pair
  = Pair RawTile
  deriving (Eq, Show)

newtype End
  = End { unEnd :: RawTile }
  deriving (Eq, Show)

mkEnd :: RawTile -> Maybe End
mkEnd = justify End end

newtype EndPair
  = EndPair { unEndPair :: RawTile }
  deriving (Eq, Show)

mkEndPair :: RawTile -> Maybe EndPair
mkEndPair = justify EndPair end

-- Formal definition of a package I made, will replace it in a later date if it
-- shows up more in my code.
justify :: (a -> b) -> (a -> Bool) -> a -> Maybe b
justify fn check a = if check a
                     then Just (fn a)
                     else Nothing
