module CHIP8.ALU where

import Clash.Prelude
import CHIP8.Types
import CHIP8.Opcode
import Data.Word

alu :: Fun -> Byte -> Byte -> (Maybe Bit, Byte)
alu fun = case fun of
    Mov -> noFlag (\x y -> y)
    Or -> noFlag (.|.)
    And -> noFlag (.&.)
    XOr -> noFlag xor
    Add -> withFlag add
    Subtract -> withFlag sub
    SubtractNeg -> withFlag (flip sub)
    ShiftRight -> withFlag (\_ y -> extend y `rotateR` 1)
    ShiftLeft -> withFlag (\_ y -> extend y `rotateL` 1)
  where
    noFlag :: (Byte -> Byte -> Byte) -> (Byte -> Byte -> (Maybe Bit, Byte))
    noFlag f x y = (Nothing, f x y)

    withFlag :: (Unsigned 8 -> Unsigned 8 -> Unsigned 9) -> (Byte -> Byte -> (Maybe Bit, Byte))
    withFlag f x y = (Just c, z)
      where
        (c, z) = bitCoerce (f (bitCoerce x) (bitCoerce y))

toHex :: Byte -> Addr
toHex x = extend (fromIntegral x :: Nybble) `shiftL` 3

-- | 9-bit maximal linear feedback shift register based on x^9 + x^5 + 1
-- http://en.wikipedia.org/wiki/Linear_feedback_shift_register#Some_polynomials_for_maximal_LFSRs
lfsr :: Unsigned 9 -> Unsigned 9
lfsr s = (s `rotateR` 1) `xor` b4
  where
    b = fromIntegral $ complement . lsb $ s
    b4 = b `shiftL` 4

toBCD :: Byte -> Vec 3 (Unsigned 4)
toBCD x = extend x100 :> x10 :> x1 :> Nil
  where
    coords :: Unsigned (2 + 4 + 4 + 8) -> (Unsigned 2, Unsigned 4, Unsigned 4, Unsigned 8)
    coords = bitCoerce

    (x100, x10, x1, _) = coords $ last $ iterate d8 (shift . add3) . shift $ buf0
      where
        buf0 = fromIntegral x

        shift = (`shiftL` 1)

        add3 x | x1 >= 5 = x + 0x0300
               | x10 >= 5 = x + 0x3000
               | otherwise = x
          where
            (_, x10, x1, _) = coords x
