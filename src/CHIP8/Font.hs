module CHIP8.Font (hexDigits) where

import CHIP8.Types

import Clash.Prelude
import Data.Word
import Data.Char
import qualified Data.List as L

-- | CHIP-8 bitmaps for hexadecimal digits, at 4x5 pixels. Padded to 8
-- pixel height to simplify indexing.
-- From http://laurencescotford.co.uk/?p=440
hexDigits :: Vec (16 * 8) Byte
hexDigits = concat . map (pad . fmap lineToByte) $
    ("****" :>
     "*  *" :>
     "*  *" :>
     "*  *" :>
     "****" :>
     Nil) :>

    ("  * " :>
     " ** " :>
     "  * " :>
     "  * " :>
     " ***" :>
     Nil) :>

    ("****" :>
     "   *" :>
     "****" :>
     "*   " :>
     "****" :>
     Nil) :>

    ("****" :>
     "   *" :>
     "****" :>
     "   *" :>
     "****" :>
     Nil) :>

    ("*  *" :>
     "*  *" :>
     "****" :>
     "   *" :>
     "   *" :>
     Nil) :>

    ("****" :>
     "*   " :>
     "****" :>
     "   *" :>
     "****" :>
     Nil) :>

    ("****" :>
     "*   " :>
     "****" :>
     "*  *" :>
     "****" :>
     Nil) :>

    ("****" :>
     "   *" :>
     "  * " :>
     " *  " :>
     " *  " :>
     Nil) :>

    ("****" :>
     "*  *" :>
     "****" :>
     "*  *" :>
     "****" :>
     Nil) :>

    ("****" :>
     "*  *" :>
     "****" :>
     "   *" :>
     "****" :>
     Nil) :>

    ("****" :>
     "*  *" :>
     "****" :>
     "*  *" :>
     "*  *" :>
     Nil) :>

    ("*** " :>
     "*  *" :>
     "*** " :>
     "*  *" :>
     "*** " :>
     Nil) :>

    ("****" :>
     "*   " :>
     "*   " :>
     "*   " :>
     "****" :>
     Nil) :>

    ("*** " :>
     "*  *" :>
     "*  *" :>
     "*  *" :>
     "*** " :>
     Nil) :>

    ("****" :>
     "*   " :>
     "****" :>
     "*   " :>
     "****" :>
     Nil) :>

    ("****" :>
     "*   " :>
     "****" :>
     "*   " :>
     "*   " :>
     Nil) :>
    Nil
  where
    pad = (++ repeat 0)

lineToByte :: String -> Byte
lineToByte s = L.foldl push 0 s `shiftL` 4
  where
    push :: Byte -> Char -> Byte
    push x c = x `shiftL` 1 + if isSpace c then 0 else 1
