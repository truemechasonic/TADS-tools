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


module Main (main) where


import Scads2_types
import Scads2_read
import Scads2_disasm
import Scads2_decompile
import Scads2_vmmisc
import Scads2_print

import Data.Char (chr,ord,isAlphaNum)
import System.IO (hPutStrLn,stderr)
import Data.List (sort)
import Data.Maybe (isJust,fromJust,fromMaybe)
import Numeric (showHex)
import System.Exit (exitFailure)
import System.Environment (getArgs)

import Data.Bits ((.&.),shiftR,testBit)
import Data.Word (Word8)

import Control.Monad (replicateM,unless)
import Control.Monad.State (State,get,put,runState,evalState)


readGameFile =
  do magic  <- getStringN 13
     unless (magic == "TADS2 bin\x0A\x0D\x1A\0") (error "Not a TADS2 game file")
     skip1  <- getBytes 7
     flags  <- getUByte
     skip2  <- getBytes 27
     chunks <- repeatUntilEmpty readChunk
     return (takeWhile notEOFChunk chunks)

readChunk =
  do tag  <- getString1
     next <- getDword
     pos  <- getPos
     substream (next-pos) (decodeChunk tag)

notEOFChunk (ChunkOther "$EOF") = False
notEOFChunk _ = True


decodeChunk "CMPD" = decodeCMPD
decodeChunk "FMTSTR" = decodeFMTSTR
decodeChunk "OBJ" = decodeOBJ
decodeChunk "REQ" = decodeREQ
decodeChunk "SPECWORD" = decodeSPECWORD
decodeChunk "VOC" = decodeVOC
decodeChunk "SYMTAB" = decodeSYMTAB
decodeChunk x = return (ChunkOther x)


decodeSYMTAB = repeatUntilEmpty decodeSymbol >>= return.ChunkSYMTAB

decodeSymbol =
  do nameLen <- getUByte
     type_   <- getUByte
     num     <- getUWord
     name    <- replicateM nameLen getUByte
     return (type_, num, map chr name)

{- 1 = func, 2 = obj, 3 = prop, 6 = builtin, (5 = self, 9 = inherited, 11 = reservedWord, 13 = argcount) -}


decodeOBJ = repeatUntilEmpty decodeObject >>= return.ChunkOBJ


decodeObject =
  do type_ <- getUByte
     n     <- getUWord
     size  <- getUWord
     use   <- getUWord
     body  <- getBytes use
     return (n, evalState (decodeOBJ' type_) body)

decodeOBJ' 1 =
  do body <- getFunction
     return (TadsFunction body)

decodeOBJ' 2 =
  do workspace <- getUWord
     flags     <- getUWord
     numSupers <- getUWord
     numProps  <- getUWord
     free      <- getUWord
     reset     <- getUWord
     static    <- getUWord
     supers    <- replicateM numSupers getUWord
     indexTable <- if testBit flags 1 then replicateM numProps getUWord else return []
     props     <- replicateM numProps decodeProp
     return (TadsObject (testBit flags 0) supers props)

decodeOBJ' n = return (UnknownObjType n)


decodeProp =
  do n     <- getUWord
     type_ <- getUByte
     size  <- getUWord
     dummy <- getUByte
     body  <- getBytes size
     return (n,evalState (decodePropType type_) body)


decodePropType 6 =	-- DAT_CODE
  getFunction >>= return.TadsMethod

decodePropType 14 =	-- DAT_DEMAND
  return Demand		-- ???

decodePropType 15 =	-- DAT_SYN
  getUWord >>= return.Synonym

decodePropType 16 =	-- DAT_REDIR
  getUWord >>= return.Redirect

decodePropType 17 =	-- DAT_TPL2
  do num <- getUByte
     x <- replicateM num decodeTPL2Record
     return (TPL2 x)

decodePropType n =
  getTaggedValue n >>= return.Val


decodeCMPD =
  do len <- getUWord
     limitBytes len
     strings <- repeatUntilEmpty getString2
     return (ChunkCMPD strings)


decodeFMTSTR =
  do len <- getUWord
     limitBytes len
     records <- repeatUntilEmpty decodeFMTSTRRecord
     return (ChunkFMTSTR records)

decodeFMTSTRRecord =
  do prop <- getUWord
     str  <- getString2
     return (str,prop)


decodeTPL2Record =
  do preposition <- getUWord
     verIoVerb   <- getUWord
     ioVerb      <- getUWord
     verDoVerb   <- getUWord
     doVerb      <- getUWord
     flags       <- getUByte
     getBytes 5
     return (preposition,(verIoVerb,ioVerb,verDoVerb,doVerb),flags)


decodeREQ =
  do objs <- repeatUntilEmpty getUWord
     return (ChunkREQ (filter ((/= 65535).fst) (zip objs reqNames)))

reqNames =
 ["Me", "takeVerb", "strObj", "numObj", "pardon",
  "againVerb", "init", "preparse", "parseError",
  "cmdPrompt", "parseDisambig", "parseError2",
  "parseDefault", "parseAskobj", "preparseCmd",
  "parseAskobjActor", "parseErrorParam", "commandAfterRead",
  "initRestore", "parseUnknownVerb", "parseNounPhrase",
  "postAction", "endCommand", "preCommand",
  "parseAskobjIndirect"]


decodeSPECWORD =
  do len <- getUWord
     limitBytes len
     records <- repeatUntilEmpty getSpecWord
     let sorted = [[x | (c',x) <- records, c'==c] | c <- "O,.ABXNPITMRY"]
     return (ChunkSPECWORD sorted)

getSpecWord =
  do flags <- getUByte
     word  <- getString1
     return (chr flags,word)


decodeVOC =
  repeatUntilEmpty decodeVOCEntry >>= return.ChunkVOC

decodeVOCEntry =
  do len1     <- getUWord
     len2     <- getUWord
     prpNum   <- getUWord
     objNum   <- getUWord
     classFlg <- getUWord
     a <- getStringN len1
     b <- getStringN len2
     let ab = if null b then a else a ++ ' ' : b
     return (ab,prpNum,objNum,classFlg)


getFunction = disasm >>= return.decompile


{-----------------------------------------------------------}


main =
  do args <- getArgs
     case args of
       [name] -> do story  <- readRawFile name
                    let chunks = evalState readGameFile story
                    mapM_ putStrLn (ppAll chunks)
       _ -> hPutStrLn stderr "usage: scads2 storyfile.gam"
