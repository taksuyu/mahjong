module Mahjong.Riichi.Base ( makeLenses
                           , Tile (..)
                           , Pile
                           , PlayerHand (..)
                           , Player (..)
                           , defaultPlayer
                           , playerDraw
                           , playerDiscard
                           , playerStealDiscard
                           , Round (..)
                           , Turn (..)
                           ) where

import           Control.Lens
import           Data.List             (delete)

import           Mahjong.Meld
import           Mahjong.Riichi.Player
import           Mahjong.Riichi.Round
import           Mahjong.Riichi.Turn
import           Mahjong.Tile


-- | We want to look through the list and remove the elem from that list; using
-- Right signifies that the operation was successful and that we can continue on
-- with the process.
takeFrom :: (Eq a) => a -> [a] -> String -> Either String [a]
takeFrom t ts message = if tz == ts
                        then Right tz
                        else Left message
  where
    tz = delete t ts

-- | Because the structure that a player draws from is pure, we don't have to
-- worry about side effects within the function. However, we do have to make
-- sure that at the end of the player's turn if there is no more tiles that the
-- game ends. Usually this means that the the hand (the current instance of the
-- game being played) will end in a draw unless the current player wins, or a
-- player wins off this player's discard.
playerDraw :: Tile -> Player -> Player
playerDraw t = hand . unPlayerHand %~ cons t

-- | Discarding has a few side effects that we have to watch out for like if a
-- player gives an invalid tile from the outside. To handle this we will return
-- an Either that will throw an error that will be returned to the client so
-- that they can pick a valid discard.
playerDiscard :: Tile -> Player -> Either String Player
playerDiscard t p = takeFrom t (_unPlayerHand . _hand $ p) "Tile wasn't in the Hand"
                    & _Right
                    %~ (\ newHand ->
                          p { _discardPile = t : _discardPile p
                            , _hand = PlayerHand newHand
                            }
                       )

-- | When a player steals a tile it mutates both the original player and the
-- player stealing the tile. Because stolen tiles must always be kept together
-- with the tiles from the hand and can never be discarded they have to be kept
-- separate. WARNING: This function assumes that the previous player not only
-- discarded the tile, didn't win, but that the meld given to the function is
-- correct for the stealing player.
--
-- FIXME: Make the function perfectly pure in the sense that it cannot create an
-- unexpected board state.
playerStealDiscard :: Player -> Player -> Meld -> (Player, Player)
playerStealDiscard p1 p2 m = ( p1 & discardPile .~ maybe [] snd tupledStolen
                             , p2 & stolenMelds %~ cons m
                             )
  where
    tupledStolen = p1 ^. discardPile & uncons
