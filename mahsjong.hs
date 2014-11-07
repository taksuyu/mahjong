module Mahsjong where

import           System.Random

data TileSet = Character
             | Circle
             | Bamboo
             | Wind
             | Dragon
               deriving Show

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
            , tileDora     :: Bool
            , tileShown    :: Bool
            } deriving Show

instance Eq Tile where
  a == b | tileSet a == tileSet b = tilePosition a == tilePosition b
         | otherwise = False

instance Ord Tile where
  compare a b = compare (tilePosition a) (tilePosition b)

genTileSet :: [([Int], TileSet, Bool)] -> [Tile]
genTileSet [] = []
genTileSet ((a, b, c):xs) = map (\n -> Tile n b False c) a ++ genTileSet xs

vanillaTiles :: [Tile]
vanillaTiles = concat $ replicate 4 $ genTileSet [ ([1..9], Character, False)
                                                 , ([1..9], Circle, False)
                                                 , ([1..9], Bamboo, False)
                                                 , ([1..4], Wind, False)
                                                 , ([1..3], Dragon, False)
                                                 ]

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

-- determineBreak ::  Integer
-- determineBreak = mod (lift getStdRandom (randomR (2, 12))) 4
