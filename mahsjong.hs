module Mahsjong where

import System.Random

data TileSet = Character
             | Circle
             | Bamboo
             | Wind
             | Dragon

instance Eq TileSet where
  Character == Character = True
  Circle == Circle = True
  Bamboo == Bamboo = True
  Wind == Wind = True
  Dragon == Dragon = True
  _ == _ = False

data Tile = Tile
            { tilePosition :: Int
            , tileSet      :: TileSet
            , tileShown    :: Bool
            }

instance Eq Tile where
  a == b | tileSet a == tileSet b = tilePosition a == tilePosition b
         | otherwise = False

instance Ord Tile where
  compare a b = compare (tilePosition a) (tilePosition b)

data Mahjong = Mahjong
                 -- Drawing pile - East, South, West, North, Dora
               { mahjongWall      :: ([Tile], [Tile], [Tile], [Tile], [Tile])

                 -- Determines wall break
               , mahjongWallStart :: Int

                 -- Discard piles - East, South, West, North
               , mahjongDiscard   :: ([Tile], [Tile], [Tile], [Tile])

                 -- Player hands - East, South, West, North
               , mahjongHands     :: ([Tile], [Tile], [Tile], [Tile])
               }

determineBreak :: Int
determineBreak = mod (getStdRandom (randomR (2, 12))) 4

randTileGen :: [Tile]
randTileGen = []