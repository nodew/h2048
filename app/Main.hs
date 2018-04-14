module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import H2048.Game (runGame)

main :: IO ()
main =
  playBanana
    (InWindow
      "Haskell 2048 implemention"
      (480, 640)
      (10, 10))
    white
    60
    runGame
