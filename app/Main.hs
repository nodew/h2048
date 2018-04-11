module Main where

import Reactive.Banana
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import H2048.Renderer
import H2048.Types

main :: IO ()
main = playBanana
  (InWindow
    "Haskell 2048 implemention"
    (480, 640)
    (10, 10))
  white
  60
  mainBanana

mainBanana :: Event InputEvent -> Moment (Behavior Picture)
mainBanana _  = do
  let picture :: Behavior Picture
      picture = pure $ renderApp $ GameState {
        board = Board [ [Empty, Tile 2, Tile 4, Tile 8]
                      , [Tile 16, Tile 32, Tile 64, Tile 128]
                      , [Tile 256, Tile 512, Tile 1024, Tile 2048]
                      , [Tile 4096, Tile 8192, Tile 16384, Tile 32768]
                      ]
        , score = 1000
        , status = InProgress
        }
  pure picture
