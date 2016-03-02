module Main where

import Test.Tasty

import Tests.Mahjong.Meld
-- import Tests.Tile

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Mahjong"
        [ properties, unitTests ]

unitTests :: TestTree
unitTests = unitMahjongMeld


properties :: TestTree
properties = testGroup "Properties"
             [ scProps
               -- , qcProps
             ]

scProps :: TestTree
scProps = testGroup "Properties (checked by smallcheck)"
          [ propMahjongMeld ]
