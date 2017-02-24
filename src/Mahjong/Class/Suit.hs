{-# LANGUAGE DefaultSignatures #-}

module Mahjong.Class.Suit where

-- | Properties of suited tiles.
--
-- Since any suit tile is enumerated between 1-9 you can use type inference to
-- get any suit that you need from. If you have an Enum instance for the type it
-- can also be automatically derived for you.
class Suit a where
  one, two, three, four, five, six, seven, eight, nine :: a
  default one :: Enum a => a
  one = toEnum 1

  default two :: Enum a => a
  two = toEnum 2

  default three :: Enum a => a
  three = toEnum 3

  default four :: Enum a => a
  four = toEnum 4

  default five :: Enum a => a
  five = toEnum 5

  default six :: Enum a => a
  six = toEnum 6

  default seven :: Enum a => a
  seven = toEnum 7

  default eight :: Enum a => a
  eight = toEnum 8

  default nine :: Enum a => a
  nine = toEnum 9

-- NOTE: Write up some property tests for refinement types perhaps?
--
-- Could test if Enum, Bounded, and Suited itself are all properly setup.
