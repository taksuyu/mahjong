{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

-- | Stability: Experimentalm
module Mahjong.Hand where

import Data.Data

import Data.MultiSet

import Mahjong.Tile

-- NOTE: I feel like there is a better way to talk about the concept of a Hand.
-- None of the elements talk to each other, but it isn't unreasonable to think
-- of a Hand being a Monoidal object comprised of sections pertaining to each
-- element. Any element that is already within would be collapsed together. So
-- characters and circles together would be a Hand of Characters, and Circles
-- which could take more Characters or Circles without changing the type.
--
-- Also there are instances where you wouldn't be interested in how you
-- represent the internal structure of the type, but it makes the type a pain to
-- interact with when it's abstracted out.
data Hand
  = Hand
    { characters :: MultiSet (Dora, Character)
    , circles    :: MultiSet (Dora, Circle)
    , bamboo     :: MultiSet (Dora, Bamboo)
    , winds      :: MultiSet (Dora, Wind)
    , dragons    :: MultiSet (Dora, Dragon) }
  deriving ( Show
           , Data, Typeable )

instance Monoid Hand where
  mempty = Hand empty empty empty empty empty
  Hand a b c d e `mappend` Hand l m n o p
    = Hand (a `mappend` l) (b `mappend` m) (c `mappend` n) (d `mappend` o) (e `mappend` p)
