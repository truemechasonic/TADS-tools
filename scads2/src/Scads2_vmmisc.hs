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


module Scads2_vmmisc (
	_and_, _or_,
	getJumpTargets,
	negateExpr,
	getTaggedValue,
	opBYTE,opWORD,opQUAD,opOBJ,opFUNC,
	opPROP,opBIF,opLIST,opSSTR,opDSTR,
	getList
) where


import Scads2_types
import Scads2_read

import Control.Monad (liftM)


_and_, _or_ :: (String,Int)

_and_ = (" && ",4)
_or_ = (" || ",3)


getJumpTargets :: [Instr] -> [Int]

getJumpTargets = concatMap getJumpTargets'

getJumpTargets' (JMP  n)  = [n]
getJumpTargets' (JT   n)  = [n]
getJumpTargets' (JS _ n)  = [n]
getJumpTargets' (BLOCK _ body) = getJumpTargets body
getJumpTargets' (THENELSE yes no) = concatMap getJumpTargets' (yes ++ no)
getJumpTargets' _ = []


negateExpr (Unary "!" exp) = exp

negateExpr (Binary left (op,prec) right) | Just po <- op `lookup` binopNegatives =
  Binary left (po, prec) right

negateExpr (Binary left op right) | op == _and_ || op == _or_ =
  Binary (negateExpr left) (if op == _and_ then _or_ else _and_) (negateExpr right)

negateExpr (IfThenExpr a b c) = IfThenExpr a (negateExpr b) (negateExpr c)

negateExpr exp = Unary "!" exp

binopNegatives =
 [(" == "," != "),(" != "," == "),(" <= "," > "),
  (" > "," <= "),(" >= "," < "),(" < "," >= ")]


getTaggedValue :: Int -> StreamReader Value

getTaggedValue 1 = opQUAD
getTaggedValue 2 = opOBJ
getTaggedValue 3 = opSSTR
getTaggedValue 5 = return (C "nil")
getTaggedValue 7 = opLIST
getTaggedValue 8 = return (C "true")
getTaggedValue 9 = opDSTR
getTaggedValue 10 = opFUNC
getTaggedValue 11 = return (C "{{{unknown 11}}}") -- DAT_TPL
getTaggedValue 12 = return (C "{{{unknown 12}}}")
getTaggedValue 13 = opPROP
getTaggedValue 14 = return (C "{{{unknown 14}}}") -- DAT_DEMAND
getTaggedValue 15 = return (C "{{{unknown 15}}}") -- DAT_SYN
getTaggedValue 16 = return (C "{{{unknown 16}}}") -- DAT_REDIR
getTaggedValue 17 = return (C "{{{unknown 17}}}") -- DAT_TPL2
getTaggedValue _ = return (C "{{{unknown}}}")


opBYTE,opWORD,opQUAD,opOBJ,opFUNC,
  opPROP,opBIF,opLIST,opSSTR,opDSTR :: StreamReader Value

opBYTE = liftM I    getUByte
opWORD = liftM I    getSWord
opQUAD = liftM I    getDword
opOBJ  = liftM OBJ  getUWord
opFUNC = liftM FUNC getUWord
opPROP = liftM PROP getUWord
opBIF  = liftM BUILTIN getUWord
opLIST = liftM LIST getList
opSSTR = liftM S    getString2
opDSTR = liftM SS   getString2


getList = getGenericList getUWord 2 (getUByte >>= getTaggedValue)
