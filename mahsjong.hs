module Mahsjong where

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
  isEnd a | a == minBound, a == maxBound = True
                              | otherwise = False

  doraIndicator :: (Eq a, Enum a, Bounded a) => a -> a
  doraIndicator a | a /= maxBound = succ a
                  | otherwise = minBound

{-
data Mahjong = Mahjong
                 -- Drawing pile - East, South, West, North, Dora
               { mahjongWall      :: ([Tile], [Tile], [Tile], [Tile], [Tile])

                 -- Determines wall break
               , mahjongWallStart :: Int

                 -- Discard piles - East, South, West, North
               , mahjongDiscard   :: ([Tile], [Tile], [Tile], [Tile])

                 -- Player hands - East, South, West, North
               , mahjongHand      :: ([Tile], [Tile], [Tile], [Tile])
               }

-- determineBreak ::  Integer
-- determineBreak = mod (lift getStdRandom (randomR (2, 12))) 4
-}
