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


module Scads3_disasm (
	tadsCodeAt,
	getDataHolder, listAt	-- these are here to avoid circular deps
) where

import Scads3_types
import Scads3_read
import Scads3_vmmisc
import Scads3_util

import Data.Array (Array,accumArray,(!))
import Numeric (showHex)
import Control.Monad (replicateM)
import Control.Monad.State (evalState)
import Data.Bits ((.&.))


{---------}


getDataHolder :: StreamReader Value

getDataHolder =
  do type_ <- getUByte
     value <- getDword
     return (decodeDataHolder type_ value)

decodeDataHolder  1 _  = C "nil"
decodeDataHolder  2 _  = C "true"
decodeDataHolder  5 n  = OBJ n
decodeDataHolder  6 n  = PROP (n .&. 65535)
decodeDataHolder  7 n  = I n
decodeDataHolder  8 n  = S (stringAt n)
decodeDataHolder  9 n  = SS (stringAt n)
decodeDataHolder 10 n  = LIST (listAt n)
decodeDataHolder 11 n  = CODE (tadsCodeAt n)
decodeDataHolder 12 n  = FUNC n
decodeDataHolder 13 _  = EMPTY
decodeDataHolder 15 n  = ENUM n
decodeDataHolder  x _  = error ("Bad DATA_HOLDER type: " ++ show x)


listAt :: Int -> [Value]

listAt n =
  evalState (do numElts <- getUWord
                replicateM numElts getDataHolder)
            (dataFrom n)


{---------}

tadsCodeAt :: Int -> TadsCode


tadsCodeAt n = (evalState getTadsCode (codeFrom n)) { tcAddr = n }

getTadsCode =
  do n <- getPos
     eos <- isEOS
     if eos then return (TadsCode undefined 0 [] [] []) else do
     argCount   <- getUByte
     reserved   <- getUByte
     localCount <- getUWord
     stackSize  <- getUWord
     exceptionTable   <- getUWord
     debuggingRecords <- getUWord
     getBytes (entpMethodHeaderSize - 10)
     (assembly,exceptions) <-
       if exceptionTable > entpMethodHeaderSize then
         do code <- getBytes (exceptionTable - entpMethodHeaderSize)
            exceptions <- getExceptionTable n
            return (evalState getInstrs code, exceptions)
       else
         do d <- getInstrs
            return (truncateCode 0 0 d, [])
     let funcsMentioned = getMentionedFunctions assembly
     return (TadsCode { tcAddr = undefined,
                        tcArgCount = argCount,
                        tcCode = assembly,
                        tcFuncsMentioned = funcsMentioned,
                        tcExceptionBlocks = exceptions })


getMentionedFunctions code = [n | PUSH (Quote (FUNC n)) <- code]


truncateCode needed seen (i@(LABEL ad _) : rest) = i : truncateCode needed ad rest
truncateCode needed seen (RETVAL : _) | needed <= seen  = [RETVAL]
truncateCode needed seen (THROW : _)  | needed <= seen  = [THROW]
truncateCode needed seen (JMP l : rest)  = JMP l : truncateCode (max l needed) seen rest
truncateCode needed seen (JT l : rest)   = JT l : truncateCode (max l needed) seen rest
truncateCode needed seen (JS b l : rest) = JS b l : truncateCode (max l needed) seen rest
truncateCode needed seen (i@(BLOCK (Switch cases) []) : rest) =
  i : truncateCode (maximum (needed : map fst cases)) seen rest
truncateCode needed seen (i : rest) = i : truncateCode needed seen rest
truncateCode needed seen [] = []


getExceptionTable addr =
  do numExceptions <- getUWord
     exceptions <- replicateM numExceptions (getException addr)
     return (groupFst exceptions)

getException addr =
  do protectFrom <- getUWord
     protectTo   <- getUWord
     exceptionObject <- getDword
     handler     <- getUWord
     getBytes (entpExceptionTableEntrySize - 10)
     return ((addr+protectFrom,addr+protectTo),(exceptionObject,addr+handler))


getInstrs :: StreamReader [Instr]

getInstrs = repeatUntilEmpty getInstr >>= return . concat


getInstr =
  do pos    <- getPos
     opcode <- getUByte
     instrs <- getInstr' opcode
     return (LABEL pos Multi : instrs)

getInstr' 0x90 =	-- SWITCH
  do cases <- getSwitchCases
     return [BLOCK (Switch cases) []]

getInstr' opcode =
  do pos <- getPos
     let opInfo@(_,params,f) = lookupOpcode opcode pos
     args <- mapM snd params
     return (f args)


getSwitchCases =
  do numCases <- getUWord
     cases    <- replicateM numCases getSwitchArg
     defaultCase <- branchOffset
     return (cases ++ [(defaultCase,Nothing)])

getSwitchArg =
  do val    <- getDataHolder
     target <- branchOffset
     return (target, Just val)


{-

disasmInstrs =
  repeatUntilEmpty disasmInstr >>= return . concat

disasmInstr =
  do pos    <- getPos
     let label = take 8 (show pos ++ ':' : repeat ' ')
     opcode <- getUByte
     case opcode of
       0x90 -> do cases <- getSwitchCases
                  let x = [replicate 10 ' ' ++ disasmSwitchCase c | c <- cases]
                  return ((label ++ "SWITCH") : x)
       _ -> do pos <- getPos
               let opInfo@(name,params,_) = lookupOpcode opcode pos
               args <- mapM snd params
               return [label ++ (take 16 (name ++ repeat ' ')) ++ join ", " (zipWith (\x y -> x ++ '=' : show y) (map fst params) args)]

disasmSwitchCase (ad,Just n) = "case " ++ ppValue n ++ " -> " ++ show ad
disasmSwitchCase (ad,Nothing) = "default -> " ++ show ad

-}


{--------------}


lookupOpcode op ad =
  case opcodeLookupTable!op of
    ("", _, _) -> error ("Invalid opcode " ++ showHex op (" at " ++ showHex ad ""))
    info       -> info


opcodeLookupTable :: Array Int MicrocodeInstr

opcodeLookupTable =
  accumArray (\a b -> b) undefined (0,255) (invalidOpcodes ++ opcodes)

invalidOpcodes =
  [(n, ("", undefined, undefined)) | n <- [0..255]]
--  [(n, ("BADOP " ++ showHex n "", [], \_ -> [BADOP n])) | n <- [0..255]]


-- The table below was machine-generated from
-- http://www.tads.org/t3spec/opcode.htm, except
-- for the translation functions.

opcodes =
 [
  (0x01,("PUSH_0",	[],			\_   -> [pushI 0])),
  (0x02,("PUSH_1",	[],			\_   -> [pushI 1])),
  (0x03,("PUSHINT8",	[("val",sbyte)],	\[v] -> [pushI v])),
  (0x04,("PUSHINT",	[("val",int4)],		\[v] -> [pushI v])),
  (0x05,("PUSHSTR",	[("offset",uint4)],	\[n] -> [pushStringAt n])),
  (0x06,("PUSHLST",	[("offset",uint4)],	\[n] -> [pushV (LIST (listAt n))])),
  (0x07,("PUSHOBJ",	[("objid",uint4)],	\[n] -> [pushOBJ n])),
  (0x08,("PUSHNIL",	[],			\_   -> [pushC "nil"])),
  (0x09,("PUSHTRUE",	[],			\_   -> [pushC "true"])),
  (0x0A,("PUSHPROPID",	[("propid",uint2)],	\[n] -> [pushV (PROP n)])),
  (0x0B,("PUSHFNPTR",	[("code_offset",uint4)],\[n] -> [pushV (FUNC n)])),

--(0x0C,("PUSHSTRI",	[("string_length",uint2)])),

-- FIXME: ignores fixed_arg_count; is this safe?
  (0x0D,("PUSHPARLST",	[("fixed_arg_count",ubyte)],	\_ -> [pushC "args"])),
  (0x0E,("MAKELSTPAR",	[],				\_ -> [MAKELSTPAR])),

  (0x0F,("PUSHENUM",	[("val",int4)],	\[n] -> [pushV (ENUM n)])),

  (0x20,("NEG",		[],	\_ -> [UNARY "-"])),
  (0x21,("BNOT",	[],	\_ -> [UNARY "~"])),
  (0x22,("ADD",		[],	\_ -> [BINARY (" + ",12)])),
  (0x23,("SUB",		[],	\_ -> [BINARY (" - ",12)])),
  (0x24,("MUL",		[],	\_ -> [BINARY (" * ",13)])),
  (0x25,("BAND",	[],	\_ -> [BINARY (" & ",8)])),
  (0x26,("BOR",		[],	\_ -> [BINARY (" | ",6)])),
  (0x27,("SHL",		[],	\_ -> [BINARY (" << ",11)])),
  (0x28,("SHR",		[],	\_ -> [BINARY (" >> ",11)])),
  (0x29,("XOR",		[],	\_ -> [BINARY (" ^ ",7)])),
  (0x2A,("DIV",		[],	\_ -> [BINARY (" / ",13)])),
  (0x2B,("MOD",		[],	\_ -> [BINARY (" % ",13)])),
  (0x2C,("NOT",		[],	\_ -> [UNARY "!"])),
  (0x2D,("BOOLIZE",	[],	\_ -> [BOOLIZE])),
  (0x2E,("INC",		[],	\_ -> [pushI 1, BINARY (" + ",12)])),
  (0x2F,("DEC",		[],	\_ -> [pushI 1, BINARY (" - ",12)])),
  (0x40,("EQ",		[],	\_ -> [BINARY (" == ",9)])),
  (0x41,("NE",		[],	\_ -> [BINARY (" != ",9)])),
  (0x42,("LT",		[],	\_ -> [BINARY (" < ",10)])),
  (0x43,("LE",		[],	\_ -> [BINARY (" <= ",10)])),
  (0x44,("GT",		[],	\_ -> [BINARY (" > ",10)])),
  (0x45,("GE",		[],	\_ -> [BINARY (" >= ",10)])),
  (0x50,("RETVAL",	[],	\_ -> [RETVAL])),
  (0x51,("RETNIL",	[],	\_ -> [pushC "nil", RETVAL])),
  (0x52,("RETTRUE",	[],	\_ -> [pushC "true", RETVAL])),
  (0x54,("RET",		[],	\_ -> [PUSHR0, RETVAL])),

  (0x58,("CALL",	[("arg_count",ubyte),("func_offset",uint4)],	\[argc,ofs] -> [pushV (FUNC ofs), CALL argc])),
  (0x59,("PTRCALL",	[("arg_count",ubyte)],				\[argc]     -> [CALL argc])),
  (0x60,("GETPROP",	[("prop_id",uint2)],				\[id]       -> [pushPROP id, CALLPROP "" 0])),
  (0x61,("CALLPROP",	[("arg_count",ubyte),("prop_id",uint2)],	\[argc,id]  -> [pushPROP id, CALLPROP "" argc])),
  (0x62,("PTRCALLPROP",	[("arg_count",ubyte)],				\[argc]     -> [CALLPROP "" argc])),
  (0x63,("GETPROPSELF",	[("prop_id",uint2)],				\[id]       -> [pushC "self", pushPROP id, CALLPROP "" 0])),
  (0x64,("CALLPROPSELF",[("arg_count",ubyte),("prop_id",uint2)],	\[argc,id]  -> [pushC "self", pushPROP id, CALLPROP "" argc])),
  (0x65,("PTRCALLPROPSELF",	[("arg_count",ubyte)],			\[argc]     -> [pushC "self", SWAP, CALLPROP "" argc])),
  (0x66,("OBJGETPROP",	[("obj_id",uint4),("prop_id",uint2)],		\[objID,id] -> [pushOBJ objID, pushPROP id, CALLPROP "" 0])),
  (0x67,("OBJCALLPROP",	[("arg_count",ubyte),("obj_id",uint4),("prop_id",uint2)],
									\[argc,objID,id] -> [pushOBJ objID, pushPROP id, CALLPROP "" argc])),
--(0x68,("GETPROPDATA",	[("prop_id",uint2)])),
--(0x69,("PTRGETPROPDATA",	[])),

  (0x6A,("GETPROPLCL1",	[("local_number",ubyte),("prop_id",uint2)],			\[l,id]      -> [pushLocal l, pushPROP id, CALLPROP "" 0])),
  (0x6B,("CALLPROPLCL1",[("arg_count",ubyte),("local_number",ubyte),("prop_id",uint2)],	\[argc,l,id] -> [pushLocal l, pushPROP id, CALLPROP "" argc])),
  (0x6C,("GETPROPR0",	[("prop_id",uint2)],				\[id]      -> [PUSHR0, pushPROP id, CALLPROP "" 0])),
  (0x6D,("CALLPROPR0",	[("arg_count",ubyte),("prop_id",uint2)],	\[argc,id] -> [PUSHR0, pushPROP id, CALLPROP "" argc])),

  (0x72,("INHERIT",	[("arg_count",ubyte),("prop_id",uint2)],		\[argc,id]  -> [pushC "inherited", pushPROP id, CALLPROP "" argc])),
  (0x73,("PTRINHERIT",	[("arg_count",ubyte)],					\[argc]     -> [pushC "inherited", SWAP, CALLPROP "" argc])),
-- FIXME: args are in a different order from OBJCALLPROP: is this correct?
  (0x74,("EXPINHERIT",	[("arg_count",ubyte),("prop_id",uint2),("obj_id",uint4)],\[argc,id,objID] -> [pushOBJ objID, pushPROP id, CALLPROP "inherited " argc])),
  (0x75,("PTREXPINHERIT",[("arg_count",ubyte),("obj_id",uint4)],		\[argc,objID] -> [pushOBJ objID, SWAP, CALLPROP "inherited " argc])),

  (0x76,("VARARGC",	[],	\_ -> [VARARGC])),

  (0x77,("DELEGATE",	[("arg_count",ubyte),("prop_id",uint2)],	\[argc,id]  -> [pushPROP id, CALLPROP "delegated " argc])),
  (0x78,("PTRDELEGATE",	[("arg_count",ubyte)],				\[argc]     -> [CALLPROP "delegated " argc])),

  (0x80,("GETLCL1",	[("local_number",ubyte)],	\[l] -> [pushV (Local l)])),
  (0x81,("GETLCL2",	[("local_number",uint2)],	\[l] -> [pushV (Local l)])),
  (0x82,("GETARG1",	[("param_number",ubyte)],	\[l] -> [pushV (Arg l)])),
  (0x83,("GETARG2",	[("param_number",uint2)],	\[l] -> [pushV (Arg l)])),
  (0x84,("PUSHSELF",	[],				\_   -> [pushC "self"])),

--(0x85,("GETDBLCL",	[("local_number",uint2),("stack_level",uint2)])),
--(0x86,("GETDBARG",	[("param_number",uint2),("stack_level",uint2)])),

  (0x87,("GETARGC",	[],			\[] -> [pushC "argcount"])),

  (0x88,("DUP",		[],	\_ -> [DUP])),
  (0x89,("DISC",	[],	\_ -> [DISC])),
  (0x8A,("DISC1",	[("count",ubyte)],	\[n] -> replicate n DISC)),
  (0x8B,("GETR0",	[],	\_ -> [PUSHR0])),

--(0x8C,("GETDBARGC",	[("stack_level",uint2)])),

  (0x8D,("SWAP",	[],	\_ -> [SWAP])),

  (0x8E,("PUSHCTXELE",	[("element",ubyte)],	\[e] -> [pushC (nameCtxEle e)])),

--(0x90,("SWITCH",	[("case_count",uint2)])),

  (0x91,("JMP",		[("branch_offset",branchOffset)],	\[ofs] -> [JMP ofs])),
  (0x92,("JT",		[("branch_offset",branchOffset)],	\[ofs] -> [JT ofs])),
  (0x93,("JF",		[("branch_offset",branchOffset)],	\[ofs] -> [UNARY "!", JT ofs])),
  (0x94,("JE",		[("branch_offset",branchOffset)],	\[ofs] -> [BINARY (" == ",9), JT ofs])),
  (0x95,("JNE",		[("branch_offset",branchOffset)],	\[ofs] -> [BINARY (" != ",9), JT ofs])),
  (0x96,("JGT",		[("branch_offset",branchOffset)],	\[ofs] -> [BINARY (" > ",10), JT ofs])),
  (0x97,("JGE",		[("branch_offset",branchOffset)],	\[ofs] -> [BINARY (" >= ",10), JT ofs])),
  (0x98,("JLT",		[("branch_offset",branchOffset)],	\[ofs] -> [BINARY (" < ",10), JT ofs])),
  (0x99,("JLE",		[("branch_offset",branchOffset)],	\[ofs] -> [BINARY (" <= ",10), JT ofs])),
  (0x9A,("JST",		[("branch_offset",branchOffset)],	\[ofs] -> [JS True ofs])),
  (0x9B,("JSF",		[("branch_offset",branchOffset)],	\[ofs] -> [JS False ofs])),
  (0x9C,("LJSR",	[("branch_offset",branchOffset)],	\[ofs] -> [LJSR ofs])),
  (0x9D,("LRET",	[("local_variable_number",int2)],\[l]  -> [LRET l])),
  (0x9E,("JNIL",	[("branch_offset",branchOffset)],	\[ofs] -> [pushC "nil", BINARY (" == ",9), JT ofs])),
  (0x9F,("JNOTNIL",	[("branch_offset",branchOffset)],	\[ofs] -> [pushC "nil", BINARY (" != ",9), JT ofs])),
  (0xA0,("JR0T",	[("branch_offset",branchOffset)],	\[ofs] -> [PUSHR0, JT ofs])),
  (0xA1,("JR0F",	[("branch_offset",branchOffset)],	\[ofs] -> [PUSHR0, UNARY "!", JT ofs])),

  (0xB0,("SAY",		[("offset",uint4)],		\[n] -> [pushStringAt n, SAYVAL])),

  (0xB1,("BUILTIN_A",	[("argc",ubyte),("func_index",ubyte)],	\[argc,func] -> [pushBUILTIN (0,func), CALL argc])),
  (0xB2,("BUILTIN_B",	[("argc",ubyte),("func_index",ubyte)],	\[argc,func] -> [pushBUILTIN (1,func), CALL argc])),
  (0xB3,("BUILTIN_C",	[("argc",ubyte),("func_index",ubyte)],	\[argc,func] -> [pushBUILTIN (2,func), CALL argc])),
  (0xB4,("BUILTIN_D",	[("argc",ubyte),("func_index",ubyte)],	\[argc,func] -> [pushBUILTIN (3,func), CALL argc])),
  (0xB5,("BUILTIN1",	[("argc",ubyte),("func_index",ubyte),("set_index",ubyte)],	\[argc,func,i] -> [pushBUILTIN (i,func), CALL argc])),
  (0xB6,("BUILTIN2",	[("argc",ubyte),("func_index",uint2),("set_index",ubyte)],	\[argc,func,i] -> [pushBUILTIN (i,func), CALL argc])),

--(0xB7,("CALLEXT",	[])),

  (0xB8,("THROW",	[],	\_ -> [THROW])),

  (0xB9,("SAYVAL",	[],	\_ -> [SAYVAL])),
  (0xBA,("INDEX",	[],	\_ -> [INDEX])),
  (0xBB,("IDXLCL1INT8",	[("local_number",ubyte),("index_val",ubyte)],	\[l,i] -> [pushLocal l, pushI i, INDEX])),
  (0xBC,("IDXINT8",	[("index_val",ubyte)],				\[i] -> [pushI i, INDEX])),

  (0xC0,("NEW1",	[("arg_count",ubyte),("metaclass_id",ubyte)],	\[argc,metaclass] -> [pushNEW True metaclass, CALL argc])),
  (0xC1,("NEW2",	[("arg_count",uint2),("metaclass_id",uint2)],	\[argc,metaclass] -> [pushNEW True metaclass, CALL argc])),
  (0xC2,("TRNEW1",	[("arg_count",ubyte),("metaclass_id",ubyte)],	\[argc,metaclass] -> [pushNEW False metaclass, CALL argc])),
  (0xC3,("TRNEW2",	[("arg_count",uint2),("metaclass_id",uint2)],	\[argc,metaclass] -> [pushNEW False metaclass, CALL argc])),

  (0xD0,("INCLCL",	[("local_number",uint2)],		\[l] -> [pushLocal l, pushI 1, BINARY (" + ",12), pushLocal l, SET])),
  (0xD1,("DECLCL",	[("local_number",uint2)],		\[l] -> [pushLocal l, pushI 1, BINARY (" - ",12), pushLocal l, SET])),
  (0xD2,("ADDILCL1",	[("local_number",ubyte),("val",sbyte)],	\[l,v] -> [pushLocal l, pushI v, BINARY (" + ",12), pushLocal l, SET])),
  (0xD3,("ADDILCL4",	[("local_number",uint2),("val",int4)],	\[l,v] -> [pushLocal l, pushI v, BINARY (" + ",12), pushLocal l, SET])),
  (0xD4,("ADDTOLCL",	[("local_number",uint2)],		\[l] -> [pushLocal l, SWAP, BINARY (" + ",12), pushLocal l, SET])),
  (0xD5,("SUBFROMLCL",	[("local_number",uint2)],		\[l] -> [pushLocal l, SWAP, BINARY (" - ",12), pushLocal l, SET])),
  (0xD6,("ZEROLCL1",	[("local_number",ubyte)],		\[l] -> [pushI 0, pushLocal l, SET])),
  (0xD7,("ZEROLCL2",	[("local_number",uint2)],		\[l] -> [pushI 0, pushLocal l, SET])),
  (0xD8,("NILLCL1",	[("local_number",ubyte)],		\[l] -> [pushC "nil", pushLocal l, SET])),
  (0xD9,("NILLCL2",	[("local_number",uint2)],		\[l] -> [pushC "nil", pushLocal l, SET])),
  (0xDA,("ONELCL1",	[("local_number",ubyte)],		\[l] -> [pushI 1, pushLocal l, SET])),
  (0xDB,("ONELCL2",	[("local_number",uint2)],		\[l] -> [pushI 1, pushLocal l, SET])),

  (0xE0,("SETLCL1",	[("local_number",ubyte)],	\[l] -> [pushLocal l, SET])),
  (0xE1,("SETLCL2",	[("local_number",uint2)],	\[l] -> [pushLocal l, SET])),
  (0xE2,("SETARG1",	[("arg_number",ubyte)],		\[a] -> [pushArg a, SET])),
  (0xE3,("SETARG2",	[("arg_number",uint2)],		\[a] -> [pushArg a, SET])),

  (0xE4,("SETIND",	[],				\_   -> [SETIND])),

  (0xE5,("SETPROP",	[("prop_id",uint2)],	\[id] -> [pushPROP id, CALLPROP "" 0, PUSHR0, SET])),
  (0xE6,("PTRSETPROP",	[],			\_ -> [CALLPROP "" 0, PUSHR0, SET])),
  (0xE7,("SETPROPSELF",	[("prop_id",uint2)],	\[id] -> [pushC "self", pushPROP id, CALLPROP "" 0, PUSHR0, SET])),
  (0xE8,("OBJSETPROP",	[("obj",uint4),("prop_id",uint2)],	\[objID,id] -> [pushOBJ objID, pushPROP id, CALLPROP "" 0, PUSHR0, SET])),

--(0xE9,("SETDBLCL",	[("local_number",uint2),("stack_level",uint2)])),
--(0xEA,("SETDBARG",	[("param_number",uint2),("stack_level",uint2)])),

  (0xEB,("SETSELF",	[],	\_ -> [pushC "self", SET])),

--(0xEC,("LOADCTX",	[])),
--(0xED,("STORECTX",	[])),

  (0xEE,("SETLCL1R0",	[("local_number",ubyte)],	\[l] -> [PUSHR0, pushLocal l, SET])),
  (0xEF,("SETINDLCL1I8",[("local_number",ubyte),("index_val",ubyte)],	\[l,i] -> [pushLocal l, pushI i, SETIND, pushLocal l, SET])),

--(0xF1,("BP",		[])),

  (0xF2,("NOP",		[],	\_ -> [NOP]))
 ]


ubyte = getUByte
sbyte = getSByte
uint2 = getUWord
int2  = getSWord
uint4 = getDword
int4  = getDword

branchOffset =
  do pos <- getPos
     ofs <- getSWord
     return (pos+ofs)


pushV v       = PUSH  (Quote v)
pushC c       = pushV (C c)
pushI i       = pushV (I i)
pushPROP p    = pushV (PROP p)
pushOBJ o     = pushV (OBJ o)
pushBUILTIN b = pushV (BUILTIN b)
pushNEW p mc  = pushV (NEW p mc)
pushLocal l   = pushV (Local l)
pushArg a     = pushV (Arg a)
pushStringAt n= pushV (S (stringAt n))


nameCtxEle 1 = "targetprop"
nameCtxEle 2 = "targetobj"
nameCtxEle 3 = "definingobj"
nameCtxEle n = "{{{CTXELE" ++ show n ++ "}}}"
