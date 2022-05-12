{-
Scads 3, a decompiler for TADS 3 game files.
Release 1, serial 040131.
Copyright 2004 Ben Rudiak-Gould.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You can read the GNU General Public License at this URL:
     http://www.gnu.org/copyleft/gpl.html
-}


module Scads3_types (
	DataBlock,
	StreamReader,
	MicrocodeInstr,
	Value(..),
	Instr(..),
	BlockType(..),
	LabelType(..),
	PartialCallType(..),
	Expr(..),
	TadsCode(..),
	Object(..),
	GrammarToken(..)
) where

import Data.Word (Word8)
import Control.Monad.State (State)
import Foreign.Ptr (Ptr)


type DataBlock = (Ptr Word8, Int, Int)

type StreamReader a = State DataBlock a


type MicrocodeInstr =
  (String, [(String,StreamReader Int)], [Int] -> [Instr])


data Value =
  C String | OBJ Int | PROP Int |
  I Int | S String | SS String | ENUM Int | LIST [Value] |
  CODE TadsCode | FUNC Int | BUILTIN (Int,Int) | NEW Bool Int |
  Local Int | SharedLocal Int | Arg Int | EMPTY
  deriving (Show,Eq)


data Instr =
  PUSH Expr | PUSHR0 | DISC | SWAP | DUP |
  UNARY String | BINARY (String,Int) | BOOLIZE | INDEX |
  CALL Int | CALLPROP String Int | RETVAL | VARARGC |
  MAKELSTPAR | NOP |
  JMP Int | JT Int | JS Bool Int | JIN Bool [Expr] Int | CONTBREAK String Int |
  LABEL Int LabelType | CASE (Maybe Value) |
  BLOCK BlockType [Instr] | THENELSE [Instr] [Instr] |
  SET | SETIND |
  SAYVAL | THROW | LJSR Int | LRET Int |
  PartialCall PartialCallType Int [Expr] |
  BADOP Int
  deriving (Show,Eq)


data BlockType =
  InfiniteLoop | Switch [(Int,Maybe Value)] | Try | Catch Int | Finally
  deriving (Show,Eq)


data LabelType = Single | Multi | Phantom  deriving (Show,Eq)


data PartialCallType = PartialFunc Expr | PartialProp String Expr Expr
  deriving (Show,Eq)


data Expr =
  Unary String Expr | Binary Expr (String,Int) Expr | Postfix Expr String |
  IfThenExpr Expr Expr Expr | Index Expr Expr | IsIn Expr Bool [Expr] |
  Call Expr [Expr] | CallProp String Expr Expr [Expr] | Quote Value |
  Assign Expr Expr |
  LstPar Expr |
  AnonFunc TadsCode
  deriving (Show,Eq)


-- addr, argCount, code, functionsMentioned
data TadsCode =
  TadsCode {
    tcAddr     :: Int,
    tcArgCount :: Int,
    tcCode     :: [Instr],
    tcFuncsMentioned  :: [Int],
    tcExceptionBlocks :: [((Int,Int),[(Int,Int)])]
  } deriving Show


-- instance Show TadsCode where
--   show tc = "{{{Code " ++ show (tcAddr tc) ++ "}}}"

instance Eq TadsCode where
  a == b  =  tcAddr a == tcAddr b

instance Ord TadsCode where
  a `compare` b  =  tcAddr a `compare` tcAddr b


data Object = TadsObject (Bool, [Int], [(Int,Value)])
            | GrammarProd [(Int,Int,Int,[GrammarToken])]
            | OtherObject (String, Bool, DataBlock)
            deriving Show


data GrammarToken =
  GramProduction Int | GramPartOfSpeech Int | GramPartOfSpeechList [Int] |
  GramLiteral String | GramTokenType Int | GramStar  deriving Show
