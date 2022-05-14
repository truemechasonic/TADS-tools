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


module Scads2_disasm (
	disasm
) where


import Scads2_types
import Scads2_read
import Scads2_vmmisc

import Control.Monad (replicateM,when)
import Control.Monad.State (evalState)
import Data.Bits((.&.),shiftR,testBit)
import Numeric (showHex)


pushV v = PUSH (Quote v)
pushC c       = pushV (C c)
pushI i       = pushV (I i)
pushPROP p    = pushV (PROP p)
pushOBJ o     = pushV (OBJ o)
pushLocal l   = pushV (Local l)


disasm :: StreamReader [Instr]
disasm = disasm' >>= return . truncateCode 0 0

disasm' = repeatUntilEmpty disasmInstr >>= return.concat


truncateCode needed seen (i@(LABEL ad _) : rest) = i : truncateCode needed ad rest
truncateCode needed seen (RETVAL : _) | needed <= seen  = [RETVAL]
truncateCode needed seen (RETURN : _) | needed <= seen  = []
truncateCode needed seen (JMP l : rest)  = JMP l : truncateCode (max l needed) seen rest
truncateCode needed seen (JT l : rest)   = JT l : truncateCode (max l needed) seen rest
truncateCode needed seen (JS b l : rest) = JS b l : truncateCode (max l needed) seen rest
truncateCode needed seen (i@(BLOCK _ body) : rest) =
  i : truncateCode (maximum (needed : getJumpTargets body)) seen rest
truncateCode needed seen (i : rest) = i : truncateCode needed seen rest
truncateCode needed seen [] = []


disasmInstr =
  do addr <- getPos
     op   <- getUByte
     acts <- if op == 0x4B then
               disasmSwitch
             else if op < 192 then
               case op `lookup` nonAssignmentOpcodes of
                 Nothing -> error ("Invalid bytecode opcode " ++ showHex op "")
                 Just (name,params,func) ->
                   do args <- sequence params
                      return (func args)
             else
               assignmentOp op
     return (LABEL addr Multi : acts)


disasmSwitch =
  do ofs <- getSWord
     when (ofs < 2) (error "Bad switch table offset")
     switchBody <- getBytes (ofs-2)
     numCases <- getUWord
     cases <- replicateM numCases disasmCase
     pos' <- getPos
     defaultCase <- getRelWord
     tableEnd <- getPos
     let cases' = cases ++ [(defaultCase,Nothing)]
     return [BLOCK (Switch [])
                   (dropJumpAroundTable tableEnd
                     (addCases cases' (evalState disasm' switchBody)))]

dropJumpAroundTable target [JMP target'] | target == target'  = []
dropJumpAroundTable target (x:xs) = x : dropJumpAroundTable target xs
dropJumpAroundTable target [] = [LABEL target Phantom]

addCases [] body = body
addCases [(defaultCase,Nothing)] [] = []	-- FIXME?
-- addCases [(defaultCase,Nothing)] [] = [(defaultCase,CASE Nothing),(defaultCase,NOP)]
addCases ((ad',val):cases) instrs@(LABEL ad _ : _) | ad' == ad  =
  CASE val : addCases cases instrs
addCases cases (instr:instrs) = instr : addCases cases instrs


disasmCase =
  do push <- disasmInstr
     case push of
       [LABEL _ _, PUSH (Quote val)] ->
            do label <- getRelWord
               return (label, Just val)
       _ -> error "Invalid case in switch statement"


opRET  = getSWord >> return (error "Bug: RETURN argument used")

opLABEL = do
  pos <- getPos
  ofs <- getSWord
  return (I (pos+ofs))

opLINE = do
  len <- getUByte
  getBytes (len-1)
  return undefined

opFRAME = do
  len <- getUWord
  getBytes (len-2)
  return undefined


nonAssignmentOpcodes =
 [(0,("nop",		[],		\_ -> [NOP])),	-- hack to handle trailing zeroes
  (1,("pushnum",	[opQUAD],	\[n] -> [pushV n])),
  (2,("pushobj",	[opOBJ],	\[o] -> [pushV o])),
  (3,("neg",	[],	\_ -> [UNARY "-"])),
  (4,("not",	[],	\_ -> [NOT])),
  (5,("add",	[],	\_ -> [BINARY (" + ",10)])),
  (6,("sub",	[],	\_ -> [BINARY (" - ",10)])),
  (7,("mul",	[],	\_ -> [BINARY (" * ",11)])),
  (8,("div",	[],	\_ -> [BINARY (" / ",11)])),
  (9,("and",	[],	\_ -> [BINARY _and_])),
  (10,("or",	[],	\_ -> [BINARY _or_])),
  (11,("eq",	[],	\_ -> [BINARY (" == ",8)])),
  (12,("ne",	[],	\_ -> [BINARY (" != ",8)])),
  (13,("gt",	[],	\_ -> [BINARY (" > ",8)])),
  (14,("ge",	[],	\_ -> [BINARY (" >= ",8)])),
  (15,("lt",	[],	\_ -> [BINARY (" < ",8)])),
  (16,("le",	[],	\_ -> [BINARY (" <= ",8)])),
  (17,("call",		[opBYTE,opFUNC],	\[I argc,func] -> [pushV func, CALL argc])),
  (18,("getp",		[opBYTE,opPROP],	\[I argc,prop] -> [pushV prop, CALLPROP "" argc])),
--(19,("getpdata",	[opBYTE,opPROP],	)),
  (20,("getlcl",	[opWORD],		\[I l] -> [pushLocal l])),
--(21,("ptrgetpdata",	[opBYTE],		)),
  (22,("return",	[opRET],		\_ -> [RETURN])),
  (23,("retval",	[opRET],		\_ -> [RETVAL])),
  (24,("enter",		[opWORD],		\[I n] -> [ENTER n])),
  (25,("discard",	[],			\_ -> [DISC])),
  (26,("jmp",		[opLABEL],		\[I l] -> [JMP l])),
  (27,("jf",		[opLABEL],		\[I l] -> [NOT, JT l])),
  (28,("pushself",	[],			\_ -> [pushC "self"])),
  (29,("say",		[opDSTR],		\[s] -> [pushV s])),
  (30,("builtin",	[opBYTE,opBIF],		\[I argc,n] -> [pushV n, CALL argc])),
  (31,("pushstr",	[opSSTR],		\[s] -> [pushV s])),
  (32,("pushlst",	[opLIST],		\[l] -> [pushV l])),
  (33,("pushnil",	[],			\_ -> [pushC "nil"])),
  (34,("pushtrue",	[],			\_ -> [pushC "true"])),
  (35,("pushfn",	[opFUNC],		\[n] -> [pushV n])),
--(36,("getpselfdata",	[opBYTE,opPROP],	)),
  (38,("ptrcall",	[opBYTE],		\[I argc] -> [CALL argc])),
  (39,("ptrinh",	[opBYTE],		\[I argc] -> [pushC "inherited", SWAP, CALLPROP "" argc])),
  (40,("ptrgetp",	[opBYTE],		\[I argc] -> [CALLPROP "" argc])),
  (41,("pass",		[opPROP],		\[PROP p] -> [PASS p])),
  (42,("exit",		[],			\_ -> [pushC "exit", DISC])),
  (43,("abort",		[],			\_ -> [pushC "abort", DISC])),
  (44,("askdo",		[],			\_ -> [pushC "askdo", DISC])),
  (45,("askio",		[opOBJ],		\[p] -> [pushV p, pushC "askio", CALL 1])),
  (46,("expinh",	[opBYTE,opPROP,opOBJ],	\[I argc,prop,obj] -> [pushV obj, pushV prop, CALLPROP "inherited " argc])),
  (47,("expinhptr",	[opBYTE,opOBJ],		\[I argc,obj] -> [pushV obj, SWAP, CALLPROP "inherited " argc])),
--(48,("calld",		[opBYTE,opFUNC],	)),
--(49,("getpd",		[opBYTE,opPROP],	)),
--(50,("builtind",	[opBYTE,opBIF],		)),
  (51,("je",		[opLABEL],		\[I l] -> [BINARY (" == ",8), JT l])),
  (52,("jne",		[opLABEL],		\[I l] -> [BINARY (" != ",8), JT l])),
  (53,("jgt",		[opLABEL],		\[I l] -> [BINARY (" > ",8), JT l])),
  (54,("jge",		[opLABEL],		\[I l] -> [BINARY (" >= ",8), JT l])),
  (55,("jlt",		[opLABEL],		\[I l] -> [BINARY (" < ",8), JT l])),
  (56,("jle",		[opLABEL],		\[I l] -> [BINARY (" <= ",8), JT l])),
  (57,("jnand",		[opLABEL],		\[I l] -> [BINARY _and_, NOT, JT l])),	-- FIXME?
  (58,("jnor",		[opLABEL],		\[I l] -> [BINARY _or_, NOT, JT l])),
  (59,("jt",		[opLABEL],		\[I l] -> [JT l])),
  (60,("getpself",	[opBYTE,opPROP],	\[I argc,prop] -> [pushC "self", pushV prop, CALLPROP "" argc])),
--(61,("getpslfd",	[opBYTE,opPROP],	)),
  (62,("getpobj",	[opBYTE,opOBJ,opPROP],	\[I argc,obj,prop] -> [pushV obj, pushV prop, CALLPROP "" argc])),
--(63,("getpobjd",	[opBYTE,opOBJ,opPROP],	)),
  (64,("index",		[],			\_ -> [INDEX])),
  (67,("pushpn",	[opPROP],		\[p] -> [pushV p])),
  (68,("jst",		[opLABEL],		\[I l] -> [JS True l])),
  (69,("jsf",		[opLABEL],		\[I l] -> [JS False l])),
--(70,("jmpd",		[opLABEL],		)),
  (71,("inherit",	[opBYTE,opPROP],	\[I argc,prop] -> [pushC "inherited", pushV prop, CALLPROP "" argc])),
--(72,("callext",	[opBYTE,opWORD],	)),
--(73,("dbgret",	[],			)),
  (74,("cons",		[opBYTE],		\[I argc] -> [CONS argc])),
--(75,("switch",	[opSWITCH],		)),
  (76,("argc",		[],			\_ -> [pushC "argc"])),
  (77,("chkargc",	[opBYTE],		\[I argc] -> [CHKARGC argc])),
  (78,("line",		[opLINE],		\_ -> [])),
  (79,("frame",		[opFRAME],		\_ -> [])),
--(80,("bp",		[opLINE],		)),
--(81,("getdblcl",	[opWORD,opWORD,opWORD],	)),
  (82,("getpptrself",	[opBYTE],		\[I argc] -> [pushC "self", SWAP, CALLPROP "" argc])),
  (83,("mod",	[],	\_ -> [BINARY (" % ",11)])),
  (84,("band",	[],	\_ -> [BINARY (" & ",7)])),
  (85,("bor",	[],	\_ -> [BINARY (" | ",5)])),
  (86,("xor",	[],	\_ -> [BINARY (" ^ ",6)])),
  (87,("bnot",	[],	\_ -> [UNARY "~"])),
  (88,("shl",	[],	\_ -> [BINARY (" << ",9)])),
  (89,("shr",	[],	\_ -> [BINARY (" >> ",9)])),
  (90,("new",	[],	\_ -> [UNARY "new "])),
  (91,("delete",[],	\_ -> [UNARY "delete "]))
 ]


assignmentOp n =
  let destType   = n .&. 3		-- 0 -> inline local, 1 -> inline prop of object on stack, 2 -> list[index] both on stack, 3 -> obj.prop both on stack
      operation  = (n `shiftR` 2) .&. 7	-- 0 -> :=, 1 -> +=, 2 -> -=, 3 -> *=, 4 -> /=, 5 -> ++, 6 -> --, 7 -> extended (inline byte): 1 -> %=, 2 -> &=, 3 -> |=, 4 -> ^=, 5 -> <<=, 6 -> >>=
      prefixDisc = testBit n 5		-- prefix/postfix for ++ and --, DISC after for others (according to t2disasm)
  in do
    operation' <- if operation == 7 then do x <- getUByte
                                            return (7+x)
                                    else return operation
    pushDest <- case destType of
      0 -> do n <- getSWord
              return [pushLocal n]
      1 -> do p <- getSWord
              return [pushPROP p, CALLPROP "" 0]
      2 -> return [INDEX]
      3 -> return [CALLPROP "" 0]
    let assign = getAssignment operation' prefixDisc
    return (pushDest ++ assign)

getAssignment operation prefixDisc =
  case operation of
    5 -> [INCDEC prefixDisc "++"]
    6 -> [INCDEC prefixDisc "--"]
    _ -> let asn = ASSIGN (assignments !! operation) in
           if prefixDisc then [asn,DISC] else [asn]

assignments =
 [" = ", " += ", " -= ", " *= "," /= ",undefined,undefined,
  undefined," %= "," &= ", " |= "," ^= "," <<= "," >>= "]
