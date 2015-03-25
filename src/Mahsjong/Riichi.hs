module Mahsjong.Riichi where

import           Mahsjong.Cycle
import           Mahsjong.Tile

import qualified Data.Set       as S

type Pile = [Tile]

type Wind = Cardinal

type Seat = Cardinal

data Mahjong =
  Mahjong { players :: [Player]
          , match   :: Wind    -- ^ Which match it is. Typically
                               -- `East` or `South`, and sometimes
                               -- `West`
          , rndHand :: HNum    -- ^ Decides which seat is now East
          , bonus   :: Integer -- ^ For when no player wins, or when the
                               -- dealer wins
          , wall    :: Pile    -- ^ Where players draw from
          } deriving (Show)

data Player =
  Player { seat     :: HNum -- ^ Designates what the seat placement is
                            -- and uses `rndHand` to determine the
                            -- seat `Wind`.
         , score    :: Integer
         , discards :: Pile -- ^ The tiles the player has discarded
         , hand     :: Pile -- ^ Their current hand
         } deriving (Show)

-- | Non Integer based rounds since there are only four of them.
data HNum = HOne | HTwo | HThree | HFour deriving (Eq, Ord, Enum, Bounded, Show)

-- | Allows HNum to cycle into the next `Mahjong.Riichi.round`.
instance Cycle HNum where
  next = wrapForward

  prev = wrapBackward

-- | Riichi wrapped `Tile` that has a `Bool` determining if the `Tile` is a dora.
type RTile = (Tile, Bool)

isDora :: RTile -> Bool
isDora = snd

playerSeatWind :: Mahjong -> Player -> Wind
playerSeatWind a b = iterate next East !! index
  where
    -- TODO: Make this look cleaner!
    -- seat HOne will be North when the rndHand is HTwo
    index    | handNum > seatNum = 4 + handNum - seatNum
             | seatNum > handNum = seatNum - handNum
             | otherwise         = 0

    seatNum  = (fromEnum . seat) b
    handNum  = (fromEnum . rndHand) a

-- | Returns a Set containing the Winds that are currently dominant
-- for that Player. These Winds are gotten from which seat the Player
-- is currently in and what `match` the game of mahjong is in.
prevailingWind :: Mahjong -> Player -> S.Set Wind
prevailingWind a b = S.fromList $ (match a) : (playerSeatWind a b) : []
