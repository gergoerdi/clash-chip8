{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import RetroClash.Sim.SDL
import Control.Monad.State

main :: IO ()
main = do
    videoBuf <- newBufferArray @64 @32

    flip evalStateT () $
      withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        return $ rasterizeBuffer videoBuf
  where
    videoParams = MkVideoParams
        { windowTitle = "CHIP-8"
        , screenScale = 20
        , screenRefreshRate = 60
        , reportFPS = True
        }
