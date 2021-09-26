{-# LANGUAGE OverloadedStrings #-}
module CHIP8.Sim where

import Clash.Prelude hiding ((!))

import CHIP8.Input
import CHIP8.Types

import RetroClash.Sim.SDL
import RetroClash.Keypad
import Control.Monad.IO.Class
import Data.Word
import Data.Array ((!))
import Data.Array.IO
import Options.Applicative

keyboardLayout :: Matrix 4 4 Scancode
keyboardLayout =
    (Scancode1 :> Scancode2 :> Scancode3 :> Scancode4 :> Nil) :>
    (ScancodeQ :> ScancodeW :> ScancodeE :> ScancodeR :> Nil) :>
    (ScancodeA :> ScancodeS :> ScancodeD :> ScancodeF :> Nil) :>
    (ScancodeZ :> ScancodeX :> ScancodeC :> ScancodeV :> Nil) :>
    Nil

keyboardMap :: Vec 16 Scancode
keyboardMap = scatter (repeat ScancodeUnknown) (concat layout) (concat keyboardLayout)

rasterizeVideoBuf :: (MonadIO m) => IOUArray VidY Word64 -> m (Rasterizer 64 32)
rasterizeVideoBuf vid = do
    vidArr <- liftIO $ freeze vid
    return $ rasterizePattern $ \x y ->
      let fg = (0xe7, 0xc2, 0x51)
          bg = (0x50, 0x50, 0x50)
          row = vidArr ! bitCoerce y
      in if testBit row (fromIntegral (maxBound - x)) then fg else bg

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "CHIP-8"
    , screenScale = 20
    , screenRefreshRate = 60
    , reportFPS = True
    }

options :: Parser FilePath
options = strArgument $ mconcat
    [ metavar "FILENAME"
    ]

optionsInfo = info (options <**> helper) $ mconcat
    [ fullDesc
    ]
