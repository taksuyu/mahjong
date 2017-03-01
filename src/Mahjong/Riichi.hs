{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, RecordWildCards #-}

-- | Stability: Experimental
module Mahjong.Riichi where

import Data.List                   as L (delete)
-- import Data.MultiSet               as MS

import Mahjong.Class
-- import Mahjong.Hand
import Mahjong.Player
import Mahjong.Tile

-- * Riichi tiles

type RiichiPlayer 
  = Player (Dora, RiichiTile)

data RiichiTile
  = CharacterT Character
  | CircleT Circle
  | BambooT Bamboo
  | WindT Wind
  | DragonT Dragon
  deriving (Eq, Show)

instance Tile RiichiTile where
  honor = \case
    WindT   _ -> True
    DragonT _ -> True
    _         -> False

  terminal = \case
    CharacterT a -> isBounds a
    CircleT    a -> isBounds a
    BambooT    a -> isBounds a
    _            -> True

instance Cycle RiichiTile where
  next = \case
    CharacterT a -> CharacterT (next a)
    CircleT    a -> CircleT    (next a)
    BambooT    a -> BambooT    (next a)
    WindT      a -> WindT      (next a)
    DragonT    a -> DragonT    (next a)

  prev = \case
    CharacterT a -> CharacterT (prev a)
    CircleT    a -> CircleT    (prev a)
    BambooT    a -> BambooT    (prev a)
    WindT      a -> WindT      (prev a)
    DragonT    a -> DragonT    (prev a)

isBounds :: (Eq a, Bounded a) => a -> Bool
isBounds a | a == minBound || a == maxBound = True
           | otherwise = False

-- | When you work with tiles you'll want some way to be able to check if two
-- tiles are of the same type.
sameGroup :: RiichiTile -> RiichiTile -> Bool
sameGroup (CharacterT _) (CharacterT _) = True
sameGroup (CircleT _)    (CircleT _)    = True
sameGroup (BambooT _)    (BambooT _)    = True
sameGroup (WindT _)      (WindT _)      = True
sameGroup (DragonT _)    (DragonT _)    = True
sameGroup _ _                           = False

-- * Hand

-- toHand :: [(Dora, RiichiTile)] -> Hand
-- toHand = foldr insertTile mempty
--   where
--     insertTile :: (Dora, RiichiTile) -> Hand -> Hand
--     insertTile t h = case snd t of
--       CharacterT a -> insertOver a t h
--       CircleT    a -> insertOver a t h
--       BambooT    a -> insertOver a t h
--       WindT      a -> insertOver a t h
--       DragonT    a -> insertOver a t h

--     insertOver a b = transformBi (insert $ a <$ b)

-- * Game actions

-- | We want to look through the list and remove the elem from that list; using
-- Right signifies that the operation was successful and that we can continue on
-- with the process.
takeFrom :: (Eq a) => a -> [a] -> String -> Either String [a]
takeFrom t ts message
  = if tz == ts
    then Right tz
    else Left message
  where
    tz = L.delete t ts

-- | Because the structure that a player draws from is pure, we don't have to
-- worry about side effects within the function. However, we do have to make
-- sure that at the end of the player's turn if there is no more tiles that the
-- game ends. Usually this means that the the hand (the current instance of the
-- game being played) will end in a draw unless the current player wins, or a
-- player wins off this player's discard.
playerDraw :: (Dora, RiichiTile) -> RiichiPlayer -> RiichiPlayer
playerDraw dt player@Player{..}
  = player { hand = addToHand hand dt}

-- | Discarding has a few side effects that we have to watch out for like if a
-- player gives an invalid tile from the outside. To handle this we will return
-- an Either that will throw an error that will be returned to the client so
-- that they can pick a valid discard.
playerDiscard :: (Dora, RiichiTile) -> RiichiPlayer -> Either String RiichiPlayer
playerDiscard t p
  = case takeFrom t ((\ (PlayerHand h) -> h ) . hand $ p) "Tile wasn't in the Hand" of
      Right a -> Right p { discardPile = t : discardPile p
                         , hand = PlayerHand a }
      Left a -> Left a

-- | When a player steals a tile it mutates both the original player and the
-- player stealing the tile. Because stolen tiles must always be kept together
-- with the tiles from the hand and can never be discarded they have to be kept
-- separate. WARNING: This function assumes that the previous player not only
-- discarded the tile, didn't win, but that the meld given to the function is
-- correct for the stealing player.
--
-- FIXME: Make the function perfectly pure in the sense that it cannot create an
-- unexpected board state.
-- playerStealDiscard :: Player -> Player -> Meld (Dora Tile) -> (Player, Player)
-- playerStealDiscard p1 p2 m = ( p1 & discardPile .~ maybe [] snd tupledStolen
--                              , p2 & stolenMelds %~ cons m )
--   where
--     tupledStolen = p1 ^. discardPile & uncons
