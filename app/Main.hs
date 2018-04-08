module Main where


import Graphics.Gloss
import H2048.Renderer
import H2048.Types

main :: IO ()
main = display
  (InWindow
    "Haskell 2048 implemention"     -- window title
    (480, 640)                      -- window size
    (10, 10))                       -- window position
  white                             -- background color
  picture                           -- picture to display

picture :: Picture
picture = renderApp $ GameState {
    board = Board [ [Empty, Tile 2, Tile 4, Tile 8]
                  , [Tile 16, Tile 32, Tile 64, Tile 128]
                  , [Tile 256, Tile 512, Tile 1024, Tile 2048]
                  , [Tile 4096, Tile 8192, Tile 16384, Tile 32768]
                  ]
  , score = 1000
  , status = InProgress
  }
