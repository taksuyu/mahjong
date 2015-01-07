module Mahsjong where

import           Data.Sequence as DS

import           Mahsjong.Hand
import           Mahsjong.Tile

data MTile = MTile
             { tile    :: Tile
             , isShown :: Bool
             , isDora  :: Bool
             } deriving (Show)

type Pile = Seq MTile

data Seat = East
          | South
          | West
          | North
          deriving (Eq, Enum, Show)

data Wall = Wall
            { wallTiles  :: Pile
            , doraTiles  :: Pile
              -- Where we start getting the dora tiles
            , doraOffset :: Int
            } deriving (Show)

data Player = Player
              { seat     :: Seat
              , points   :: Integer
              , hand     :: Pile
              , discards :: Pile
              , inRiichi :: Bool
              } deriving (Show)

data Mahjong = Mahjong
               { wall    :: Wall
               , players :: [Player]
               , round   :: (Seat, Int)
               } deriving (Show)
