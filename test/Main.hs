-- | Main entry file for tests.

import Test.DocTest

main :: IO ()
main = doctest ["src/Mahjong"]
