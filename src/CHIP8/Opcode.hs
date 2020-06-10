module CHIP8.Opcode
    ( Fun(..), Op(..)
    , decodeInstr
    , fatal
    ) where

import Clash.Prelude

import CHIP8.Types
import Data.Word
import Text.Printf

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
    deriving (Show, Generic, NFDataX)

data Op
    = ClearScreen
    | Ret
    | Sys Addr
    | Jump Addr
    | Call Addr
    | SkipEqImmIs Bool Reg Byte
    | SkipEqRegIs Bool Reg Reg
    | PutImm Reg Byte
    | AddImm Reg Byte
    | ALU Fun Reg Reg
    | SetPtr Addr
    | JumpPlusR0 Addr
    | Randomize Reg Byte
    | DrawSprite Reg Reg Nybble
    | SkipKeyIs Bool Reg
    | GetTimer Reg
    | WaitKey Reg
    | SetTimer Reg
    | SetSound Reg
    | AddPtr Reg
    | LoadFont Reg
    | StoreBCD Reg
    | StoreRegs Reg
    | LoadRegs Reg
    deriving (Show, Generic, NFDataX)

decodeInstr :: Byte -> Byte -> Op
decodeInstr hi lo = case codes of
    (0x0, 0x0, 0xe, 0x0) -> ClearScreen
    (0x0, 0x0, 0xe, 0xe) -> Ret
    (0x0,   _,   _,   _) -> Sys addr
    (0x1,   _,   _,   _) -> Jump addr
    (0x2,   _,   _,   _) -> Call addr
    (0x3,   x,   _,   _) -> SkipEqImmIs True x imm
    (0x4,   x,   _,   _) -> SkipEqImmIs False x imm
    (0x5,   x,   y, 0x0) -> SkipEqRegIs True x y
    (0x6,   x,   _,   _) -> PutImm x imm
    (0x7,   x,   _,   _) -> AddImm x imm
    (0x8,   x,   y, fun) -> ALU (decodeFun fun) x y
    (0x9,   x,   y, 0x0) -> SkipEqRegIs False x y
    (0xa,   _,   _,   _) -> SetPtr addr
    (0xb,   _,   _,   _) -> JumpPlusR0 addr
    (0xc,   x,   _,   _) -> Randomize x imm
    (0xd,   x,   y,   n) -> DrawSprite x y n
    (0xe,   x, 0x9, 0xe) -> SkipKeyIs True x
    (0xe,   x, 0xa, 0x1) -> SkipKeyIs False x
    (0xf,   x, 0x0, 0x7) -> GetTimer x
    (0xf,   x, 0x0, 0xa) -> WaitKey x
    (0xf,   x, 0x1, 0x5) -> SetTimer x
    (0xf,   x, 0x1, 0x8) -> SetSound x
    (0xf,   x, 0x1, 0xe) -> AddPtr x
    (0xf,   x, 0x2, 0x9) -> LoadFont x
    (0xf,   x, 0x3, 0x3) -> StoreBCD x
    (0xf,   x, 0x5, 0x5) -> StoreRegs x
    (0xf,   x, 0x6, 0x5) -> LoadRegs x
    _                    -> errorX $ "Unknown opcode: " <>
                            printf "%04x" (bitCoerce codes :: Word16)
  where
    (n1, n2) = nybbles hi
    (n3, n4) = nybbles lo
    codes = (n1, n2, n3, n4)
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
decodeFun n = fatal "Unknown ALU function" n

fatal :: (Show a) => String -> a -> b
fatal s x = errorX $ s <> ": " <> show x
