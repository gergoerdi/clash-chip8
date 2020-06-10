module CHIP8.Types where

import Clash.Prelude
import Clash.Sized.Vector
import Clash.Sized.Unsigned
import Data.Word

type Nybble = Unsigned 4
type Byte = Word8
type Addr = Unsigned 12
type Reg = Nybble

type VidX = Unsigned 6
type VidY = Unsigned 5
type VidRow = Word64

type Key = Index 16
-- type KeypadState = Vec 16 Bool

nybbles :: Byte -> (Nybble, Nybble)
nybbles = bitCoerce

-- toAddr :: Nybble -> Nybble -> Nybble -> Addr
-- toAddr a1 a2 a3 = bitCoerce (a1, a2, a3)
