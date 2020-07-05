module CHIP8.Opcode
    ( Instr(..), Fun(..)
    , decodeInstr
    ) where

import Clash.Prelude

import CHIP8.Types
import Data.Word
import Text.Printf

data Instr
    = Sys Addr
    | Jump Addr
    | JumpPlusV0 Addr
    | Call Addr
    | Ret
    | SkipEqImmIs Bool Reg Byte
    | SkipEqRegIs Bool Reg Reg
    | LoadImm Reg Byte
    | AddImm Reg Byte
    | StoreRegs Reg
    | LoadRegs Reg
    | LoadPtr Addr
    | AddPtr Reg
    | Arith Fun Reg Reg
    | Randomize Reg Byte
    | StoreBCD Reg
    | ClearScreen
    | DrawSprite Reg Reg Nybble
    | LoadHex Reg
    | SkipKeyIs Bool Reg
    | WaitKey Reg
    | LoadTimer Reg
    | GetTimer Reg
    | LoadSound Reg
    deriving (Show)

data Fun
    = Id
    | Or
    | And
    | XOr
    | Add
    | Subtract
    | ShiftRight
    | SubtractFlip
    | ShiftLeft
    deriving (Show)

decodeInstr :: Byte -> Byte -> Instr
decodeInstr hi lo = case nybbles of
    (0x0, 0x0, 0xe, 0x0) -> ClearScreen
    (0x0, 0x0, 0xe, 0xe) -> Ret
    (0x0,   _,   _,   _) -> Sys addr
    (0x1,   _,   _,   _) -> Jump addr
    (0x2,   _,   _,   _) -> Call addr
    (0x3,   x,   _,   _) -> SkipEqImmIs True x imm
    (0x4,   x,   _,   _) -> SkipEqImmIs False x imm
    (0x5,   x,   y, 0x0) -> SkipEqRegIs True x y
    (0x6,   x,   _,   _) -> LoadImm x imm
    (0x7,   x,   _,   _) -> AddImm x imm
    (0x8,   x,   y, fun) -> Arith (decodeFun fun) x y
    (0x9,   x,   y, 0x0) -> SkipEqRegIs False x y
    (0xa,   _,   _,   _) -> LoadPtr addr
    (0xb,   _,   _,   _) -> JumpPlusV0 addr
    (0xc,   x,   _,   _) -> Randomize x imm
    (0xd,   x,   y,   n) -> DrawSprite x y n
    (0xe,   x, 0x9, 0xe) -> SkipKeyIs True x
    (0xe,   x, 0xa, 0x1) -> SkipKeyIs False x
    (0xf,   x, 0x0, 0x7) -> GetTimer x
    (0xf,   x, 0x0, 0xa) -> WaitKey x
    (0xf,   x, 0x1, 0x5) -> LoadTimer x
    (0xf,   x, 0x1, 0x8) -> LoadSound x
    (0xf,   x, 0x1, 0xe) -> AddPtr x
    (0xf,   x, 0x2, 0x9) -> LoadHex x
    (0xf,   x, 0x3, 0x3) -> StoreBCD x
    (0xf,   x, 0x5, 0x5) -> StoreRegs x
    (0xf,   x, 0x6, 0x5) -> LoadRegs x
    _                    -> errorX $ printf "Unknown opcode: %02x %02x" hi lo
  where
    (n1, n2) = bitCoerce hi :: (Nybble, Nybble)
    (n3, n4) = bitCoerce lo :: (Nybble, Nybble)
    nybbles = (n1, n2, n3, n4)
    addr = bitCoerce (n2, n3, n4)
    imm = lo

decodeFun :: Nybble -> Fun
decodeFun 0x0 = Id
decodeFun 0x1 = Or
decodeFun 0x2 = And
decodeFun 0x3 = XOr
decodeFun 0x4 = Add
decodeFun 0x5 = Subtract
decodeFun 0x6 = ShiftRight
decodeFun 0x7 = SubtractFlip
decodeFun 0xe = ShiftLeft
decodeFun n = errorX $ printf "Unknown arithmetic function: %x" (fromIntegral n :: Byte)
