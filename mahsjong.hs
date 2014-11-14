module Mahsjong where

import           Data.Tuple.Select
import           GHC.Enum

data CharacterTile = CharacterTile Int Bool Bool deriving Show

instance Tile CharacterTile where
  position (CharacterTile a _ _) = a
  isDora (CharacterTile _ b _ ) = b
  isShown (CharacterTile _ _ c) = c

instance Eq CharacterTile where
  CharacterTile a _ _ == CharacterTile b _ _ = a == b

instance Bounded CharacterTile where
  minBound = CharacterTile 1 False False
  maxBound = CharacterTile 9 False False

instance Enum CharacterTile where
  toEnum n | n >= 1, n <= 9 = CharacterTile n False False
  toEnum _ = error "CharacterTile.toEnum: bad argument"

  fromEnum (CharacterTile a _ _) = a

  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

class Tile a where
  -- What is the position of the tile (e.g. East wind is the first wind tile)
  position :: a -> Int

  -- Is this tile a dora?
  isDora :: a -> Bool

  -- Is this tile currently shown?
  isShown :: a -> Bool

  -- Is this tile an end? Generally they are the min and max bounds
  isEnd :: (Eq a, Bounded a) => a -> Bool
  isEnd a | a == minBound || a == maxBound = True
          | otherwise = False

  -- A dora is the tile after the given tile in the same set
  doraIndicator :: (Eq a, Enum a, Bounded a) => a -> a
  doraIndicator a | a /= maxBound = succ a
                  | otherwise = minBound

data Mahjong = Mahjong
                 -- Drawing pile - East, South, West, North, Dora
               { mahjongWall      :: Tile a => ([a], [a], [a], [a], [a])

                 -- Determines wall break
               , mahjongWallStart :: Int

                 -- Discard piles - East, South, West, North
               , mahjongDiscard   :: Tile a => ([a], [a], [a], [a])

                 -- Player hands - East, South, West, North
               , mahjongHand      :: Tile a => ([a], [a], [a], [a])
               }

-- Helper functions --
getEastWall, getSouthWall, getWestWall, getNorthWall, getDora :: Tile a => Mahjong -> [a]
getEastWall a = sel1 $ mahjongWall a
getSouthWall a = sel2 $ mahjongWall a
getWestWall a = sel3 $ mahjongWall a
getNorthWall a = sel4 $ mahjongWall a
getDora a = sel5 $ mahjongWall a

getEastDiscard, getSouthDiscard, getWestDiscard, getNorthDiscard :: Tile a => Mahjong -> [a]
getEastDiscard a = sel1 $ mahjongDiscard a
getSouthDiscard a = sel2 $ mahjongDiscard a
getWestDiscard a = sel3 $ mahjongDiscard a
getNorthDiscard a = sel4 $ mahjongDiscard a

getEastHand, getSouthHand, getWestHand, getNorthHand :: Tile a => Mahjong -> [a]
getEastHand a = sel1 $ mahjongHand a
getSouthHand a = sel2 $ mahjongHand a
getWestHand a = sel3 $ mahjongHand a
getNorthHand a = sel4 $ mahjongHand a

-- determineBreak :: Int
-- determineBreak = mod (lift getStdRandom (randomR (2, 12))) 4
