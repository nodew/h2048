{-# LANGUAGE UnicodeSyntax, Rank2Types #-}

module Graphics.Gloss.Interface.FRP.ReactiveBanana (playBanana, InputEvent) where

import Graphics.Gloss hiding (display)
import Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.IORef (newIORef, readIORef, writeIORef)

-- | A useful type synonym for Gloss event values, to avoid confusion between
--   Gloss and ReactiveBanana.
type InputEvent = G.Event

-- | Play the game in a window, updating when the value of the provided
--   Behavior t Picture changes.
playBanana ∷ Display -- ^ The display method
           -> Color   -- ^ The background colour
           -> Int     -- ^ The refresh rate, in Hertz
           -> (Event InputEvent -> MomentIO (Behavior Picture))
           -> IO ()
playBanana display colour frequency mainBanana = do
  picRef <- newIORef blank
  (eventHandler, fireEvent) <- newAddHandler

  network <-
    compile $ do
      glossEvent <- fromAddHandler eventHandler
      picture <- mainBanana glossEvent
      changes picture >>= reactimate' . fmap (fmap (writeIORef picRef))
      valueBLater picture >>= liftIO . writeIORef picRef
  actuate network

  playIO display colour frequency ()
    (\() -> readIORef picRef)
    (\e () -> fireEvent e)
    (\_ () -> pure ())
