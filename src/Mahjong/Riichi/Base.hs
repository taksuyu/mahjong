module Mahjong.Riichi.Base ( makeFields
                           , RawTile ()
                           , Pile
                           , Hand (..)
                           , drawTile
                           , discardFromHand
                           , tileToDiscard
                           , Player (..)
                           , Round (..)
                           , Turn (..)
                           ) where

import Control.Lens (makeFields)
import Mahjong.Meld
import Mahjong.Tile

type Tile = RawTile
type Pile = [Tile]

newtype Hand
  = Hand { unHand :: Pile }
  deriving (Show)

takeFrom :: (Eq a) => a -> [a] -> (Maybe a, [a])
takeFrom _ []  = (Nothing, [])
takeFrom a [x] | x == a    = (Just a, [])
               | otherwise = (Nothing, [x])
takeFrom a (x:xs) | x == a    = (Just a, xs)
                  | otherwise = let (tile, rest) = takeFrom a xs
                                in (tile, rest)

drawTile :: Pile -> Hand -> (Pile, Hand)
drawTile [] ys = ([], ys)
drawTile (x:xs) (Hand ys) = (xs, Hand (x:ys))

-- Split this function into two parts, since a tile could be stolen.
-- discardTile :: Hand -> Pile -> Tile -> Either String (Hand, Pile)
-- discardTile xs@(Hand []) ys _ = Right (xs, ys) -- Not an actual thing that should ever happen
-- discardTile (Hand xs) ys t = let (rest, tile) = takeFrom xs t
--                              in maybe (Left "error: tile doesn't exist in player's hand!")
--                                 (\ jt -> Right (Hand rest, jt:ys))
--                                 tile

-- Since a tile can be stolen we need to have a step between discardFromHand and
-- tileToDiscard to see if the tile doesn't end up in some one else hand as a
-- stolen tile.
discardFromHand :: Tile -> Hand -> (Maybe Tile, Hand)
discardFromHand t (Hand ts)= let (mt, h) = takeFrom t ts
                             in (mt, Hand h)

-- As this function is, if t is at the front then the front is actually the last
-- tile in the pile. This could be counter intuitive in implementation because
-- the front is actually the back.
tileToDiscard :: Tile -> Pile -> Pile
tileToDiscard t p = t:p

data Player
  = Player
    { score       :: Integer
    , hand        :: Hand
    , stolenMelds :: [Meld]
    , discardPile :: Pile
    }
  deriving (Show)
makeFields ''Player

-- TODO: This could probably be a lot cleaner along with discardTile
-- playerDiscard :: Player -> Tile -> Either String Player
-- playerDiscard p = either Left
--                   (\ (newHand, newDiscard) ->
--                      Right (p { hand = newHand, discardPile = newDiscard }))
--                   . discardTile (hand p) (discardPile p)

data Round
  = EastRound
  | SouthRound
  | WestRound
  | NorthRound
  deriving (Eq, Ord, Enum, Bounded, Show)

data Turn
  = EastTurn
  | SouthTurn
  | WestTurn
  | NorthTurn
  deriving (Eq, Ord, Enum, Bounded, Show)
