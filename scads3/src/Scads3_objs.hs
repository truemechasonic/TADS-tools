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


module Scads3_objs (
	objects, tadsObjects,
	getTadsObjectFuncRefs, getTadsCodeFuncRefs
) where


import Scads3_types
import Scads3_vmmisc
import Scads3_read
import Scads3_disasm (getDataHolder)

import Data.Bits (testBit)
import Control.Monad (replicateM)
import Control.Monad.State (evalState)


objects :: [(Int,Object)]
tadsObjects :: [(Int,(Bool,[Int],[(Int,Value)]))]

getTadsObjectFuncRefs :: (Bool,[Int],[(Int,Value)]) -> [Int]
getTadsObjectCodeContent :: (Bool,[Int],[(Int,Value)]) -> [TadsCode]


objects =
  map decoScadsObjects objsList where
    decoScadsObjects (id,("TadsObject",_,block)) =
      (id, TadsObject (evalState parseTadsObject block))
    decoScadsObjects (id,("GrammarProd",_,block)) =
      (id, GrammarProd (evalState parseGrammarProd block))
    decoScadsObjects (id,other) =
      (id, OtherObject other)

tadsObjects = [(id,x) | (id,TadsObject x) <- objects]


getTadsObjectCodeContent (_,_,props) =
  concatMap helper (map snd props) where
    helper (CODE x) = [x]
    helper (LIST x) = concatMap helper x
    helper _        = []

getTadsObjectFuncRefs (_,_,props) =
  concatMap helper (map snd props) where
    helper (FUNC x) = [x]
    helper (CODE x) = getTadsCodeFuncRefs x
    helper (LIST x) = concatMap helper x
    helper _        = []

getTadsCodeFuncRefs = tcFuncsMentioned


{--------------------------------------------------}

parseGrammarProd =
  do numAlternatives <- getUWord
     replicateM numAlternatives parseGrammarAlt

parseGrammarAlt =
  do score     <- getUWord
     badness   <- getUWord
     objID     <- getDword
     numTokens <- getUWord
     tokens    <- replicateM numTokens parseGrammarToken
     return (score,badness,objID,tokens)

parseGrammarToken =
  do propAssoc <- getUWord
     tokenType <- getUByte
     case tokenType of
       1 -> do			-- VMGRAM_MATCH_PROD
         prod <- getDword
         return (GramProduction prod)
       2 -> do			-- VMGRAM_MATCH_SPEECH
         pos  <- getUWord
         return (GramPartOfSpeech pos)
       3 -> do			-- VMGRAM_MATCH_LITERAL
         literal  <- getString2
         return (GramLiteral literal)
         -- dictionary???
       4 -> do			-- VMGRAM_MATCH_TOKTYPE
         enumID   <- getDword
         return (GramTokenType enumID)
       5 -> do			-- VMGRAM_MATCH_STAR
         return GramStar
       6 -> do			-- VMGRAM_MATCH_NSPEECH
         numProps <- getUWord
         pos      <- replicateM numProps getUWord
         return (GramPartOfSpeechList pos)
       _ ->
         error ("Bad token type (" ++ show tokenType ++ ") in GrammarProd")


{------------------------- TadsObject parsing and printing -------------------------}

parseTadsObject :: StreamReader (Bool, [Int], [(Int,Value)])


parseTadsObject =
  do numSupers <- getUWord
     numProps  <- getUWord
     flags     <- getUByte
     _         <- getUByte
     supers    <- replicateM numSupers getDword
     props     <- replicateM numProps decodeProperty
     let isClass = testBit flags 0
     return (isClass, supers, props)


decodeProperty =
  do id  <- getUWord
     val <- getDataHolder
     return (id,val)
