{-# LANGUAGE UnicodeSyntax, Rank2Types #-}

module Graphics.Gloss.Interface.FRP.ReactiveBanana (playBanana, InputEvent) where

import Graphics.Gloss hiding (display)
import Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Exit

type InputEvent = G.Event

playBanana
    :: Display -- ^ The display method
    -> Color   -- ^ The background colour
    -> Int     -- ^ The refresh rate, in Hertz
    -> (Event InputEvent -> MomentIO (Behavior Picture))
    -> IO ()
playBanana display colour fps mainBanana = do
    picRef                    <- newIORef blank
    (eventHandler, fireEvent) <- newAddHandler
    -- ^ exit program when `ESC` key down
    let handleEvent e@(G.EventKey k G.Down _ _) = case k of
            (G.SpecialKey G.KeyEsc) -> exitSuccess
            _                       -> fireEvent e
        handleEvent e = fireEvent e

    network <- compile $ do
        glossEvent <- fromAddHandler eventHandler
        picture    <- mainBanana glossEvent
        changes picture >>= reactimate' . fmap (fmap (writeIORef picRef))
        valueBLater picture >>= liftIO . writeIORef picRef
    actuate network

    playIO display
           colour
           fps
           ()
           (\() -> readIORef picRef)
           (\e () -> handleEvent e)
           (\_ () -> pure ())


