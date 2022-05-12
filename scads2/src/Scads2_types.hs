{-
Scads 2, a decompiler for TADS 2 game files.
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


module Scads2_types (
	DataBlock,
	StreamReader,
	Dword,
	Object(..),
	PropValue(..),
	Chunk(..),
	Instr(..),
	Value(..),
	BlockType(..),
	LabelType(..),
	PartialCallType(..),
	Expr(..)
) where


import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import Control.Monad.State (State)


type DataBlock = (Ptr Word8, Int, Int)

type StreamReader a = State DataBlock a

type Dword = Int


data Object =
  TadsFunction [Instr] |
  TadsObject Bool [Int] [(Int,PropValue)] |
  UnknownObjType Int  deriving Show


data PropValue =
  TadsMethod [Instr] | Demand | Synonym Int | Redirect Int | Val Value |
  TPL2 [(Int,(Int,Int,Int,Int),Int)]  deriving Show


data Chunk =
  ChunkCMPD [String] |
  ChunkFMTSTR [(String,Int)] |
  ChunkOBJ [(Int,Object)] |
  ChunkREQ [(Int,String)] |
  ChunkSPECWORD [[String]] |
  ChunkVOC [(String,Int,Int,Int)] |
  ChunkSYMTAB [(Int,Int,String)] |
  ChunkOther String  deriving Show


data Instr =
  PUSH Expr | DISC | SWAP | NOP | BLOCKEXIT |
  UNARY String | BINARY (String,Int) | NOT | INDEX |
  CALL Int | CALLPROP String Int | RETVAL | RETURN |
  PASS Int | ASKIO Value | CONS Int |
  JMP Int | JT Int | JS Bool Int |
  LABEL Int LabelType | CASE (Maybe Value) |
  BLOCK BlockType [Instr] | THENELSE [Instr] [Instr] |
  ASSIGN String | INCDEC Bool String |
  SAY String | NEW | DELETE |
  CHKARGC Int | ENTER Int |
  PartialCall PartialCallType Int [Expr]
  deriving (Show,Eq)


data Value =
  C String | OBJ Int | FUNC Int | PROP Int |
  I Dword | S String | SS String |
  LIST [Value] | EMPTY | BUILTIN Int | Local Int
  deriving (Show,Eq)


data BlockType = InfiniteLoop | Switch [(Int,Maybe Value)]  deriving (Show,Eq)


data LabelType = Single | Multi | Phantom  deriving (Show,Eq)


data PartialCallType = PartialFunc Expr | PartialProp String Expr Expr | PartialCons
  deriving (Show,Eq)


data Expr =
  Unary String Expr | Binary Expr (String,Int) Expr | Postfix Expr String |
  IfThenExpr Expr Expr Expr | Index Expr Expr | List [Expr] |
  Call Expr [Expr] | CallProp String Expr Expr [Expr] | Quote Value |
  Assign Expr String Expr
  deriving (Show,Eq)


