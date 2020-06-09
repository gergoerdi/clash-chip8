module CHIP8.ALU where

import Clash.Prelude
import CHIP8.Types
import CHIP8.Opcode
import Data.Word

alu :: Fun -> Byte -> Byte -> (Maybe Bit, Byte)
alu fun = case fun of
    Id -> noCarry (\x y -> y)
    Or -> noCarry (.|.)
    And -> noCarry (.&.)
    XOr -> noCarry xor
    Add -> carry add
    Subtract -> carry sub
    SubtractFlip -> carry (flip sub)
    ShiftRight -> carry (\x _ -> extend x `rotateR` 1)
    ShiftLeft -> carry (\x _ -> extend x `rotateL` 1)
  where
    noCarry :: (Byte -> Byte -> Byte) -> (Byte -> Byte -> (Maybe Bit, Byte))
    noCarry f x y = (Nothing, f x y)

    carry :: (Unsigned 8 -> Unsigned 8 -> Unsigned 9) -> (Byte -> Byte -> (Maybe Bit, Byte))
    carry f x y = let (c, z) = bitCoerce (f (bitCoerce x) (bitCoerce y)) in (Just c, z)

toBCD :: Byte -> Vec 3 Byte
toBCD x =
    x `div` 100 :>
    (x `div` 10) `mod` 10 :>
    x `mod` 10 :>
    Nil

toFont :: Byte -> Addr
toFont x = extend lo `shiftL` 3
  where
    (_, lo) = nybbles x

-- | 9-bit maximal linear feedback shift register based on x^9 + x^5 + 1
-- http://en.wikipedia.org/wiki/Linear_feedback_shift_register#Some_polynomials_for_maximal_LFSRs
lfsr :: Unsigned 9 -> Unsigned 9
lfsr s = (s `rotateR` 1) `xor` b4
  where
    b = fromIntegral $ complement . lsb $ s
    b4 = b `shiftL` 4
