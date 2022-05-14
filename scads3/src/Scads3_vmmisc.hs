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


module Scads3_vmmisc (
	objsList,
	metaclassDependencyList, lookupMetaclass, nameMetaclass,
	functionSetDependencyList,
	symdObjectNames, symdPropertyNames,
	entpMainAddr, entpMethodHeaderSize, entpExceptionTableEntrySize,
	otherChunks,
	rawFrom, codeFrom, dataFrom,
	stringAt,
	_and_, _or_, negateExpr, isBoolean
) where


import Scads3_types
import Scads3_read
import Scads3_util

import Control.Monad (replicateM)
import Control.Monad.State (evalState,execState,get,put)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe,fromJust,isJust)
import Data.Bits (testBit,(.&.))
import System.IO (hPutStrLn,stderr)
import System.Exit (exitFailure)
import System.Environment (getArgs)


storyFile = unsafePerformIO (readRawFile storyFileName)


storyFileName =
  unsafePerformIO $
    do args <- getArgs
       case args of
         [name] -> return name
         _      -> do hPutStrLn stderr "usage: scads3 file.t3"
                      exitFailure


rawFrom  :: Int -> DataBlock
rawFrom n = execState (getBytes n) storyFile


{-----------}


chunks :: [(String,[DataBlock])]

chunks =
  groupFst $ truncateChunks $
    evalState (repeatUntilEmpty getChunk) (rawFrom 69)

truncateChunks = takeWhile ((/= "EOF ") . fst)

getChunk =
  do id     <- getStringN 4
     length <- getDword
     _      <- getUWord
     body   <- getBytes length
     return (id,body)


lookupChunks name = fromMaybe [] (lookup name chunks)


{-----------}


codeFrom :: Int -> DataBlock
codeFrom n = fromMaybe (codeFault n) (poolFrom bytecodePool n)

dataFrom :: Int -> DataBlock
dataFrom n = fromMaybe (dataFault n) (poolFrom constDataPool n)

dataFault n =
  error ("Bad TADS3 file: reference to nonexistent data page (address " ++ show n ++ ")")

codeFault n =
  evalState (getBytes 0) (rawFrom 0)
--  error ("Bad TADS3 file: reference to nonexistent code page (address " ++ show n ++ ")")


poolDefs = groupFst $ map (evalState parseCPDF) $ lookupChunks "CPDF"
poolPages = groupFst $ map (evalState parseCPPG) $ lookupChunks "CPPG"

parseCPDF =
  do poolID   <- getUWord
     numPages <- getDword
     pageSize <- getDword
     return (poolID,pageSize)

parseCPPG =
  do poolID   <- getUWord
     pageIdx  <- getDword
     xorMask  <- getUByte
     rest     <- get
     return $ (poolID,(pageIdx,rest))


bytecodePool  = makePoolLookupTable 1
constDataPool = makePoolLookupTable 2

makePoolLookupTable :: Int -> (Int, [(Int, DataBlock)])

makePoolLookupTable poolID =
  case lookup poolID poolDefs of
    Just [pageSize] -> (pageSize, fromMaybe [] (lookup poolID poolPages))
    Just _  -> error ("Bad TADS3 file: >1 CPDF block with id " ++ show poolID)
    Nothing -> error ("Bad TADS3 file: no CPDF block with id " ++ show poolID)


poolFrom (pageSize,table) n =
  let (hi,lo) = n `divMod` pageSize
  in fmap (execState (getBytes lo)) (lookup hi table)


{-----------}


(entpMainAddr,entpMethodHeaderSize,entpExceptionTableEntrySize) =
  case lookupChunks "ENTP" of
    (x:_) -> evalState parseENTP x
    []    -> error "Bad TADS3 file: no ENTP block"


parseENTP =
  do mainRoutine      <- getDword
     methodHdrSize    <- getUWord
     exceptionTblSize <- getUWord
     debugLineTblSize <- getUWord
     debugTblHdrSize  <- getUWord
     debugSymbolRecordHdrSize <- getUWord
     debugRecordVersion       <- getUWord
     return (mainRoutine,methodHdrSize,exceptionTblSize)


{-----------}


-- (id,(metaclass,persistent,data))
objsList :: [(Int,(String,Bool,DataBlock))]

objsList =
  concatMap (evalState parseOBJS) (lookupChunks "OBJS")

parseOBJS =
  do numObjs   <- getUWord
     metaclass <- getUWord
     flags     <- getUWord
     let mc      = nameMetaclass metaclass
         large   = testBit flags 0
         persist = not (testBit flags 1)
     objs      <- replicateM numObjs (parseObj large)
     return [(id,(mc,persist,body)) | (id,body) <- objs]

parseObj large =
  do objID <- getDword
     len   <- if large then getDword else getUWord
     body  <- getBytes len
     return (objID,body)


{-----------}


metaclassDependencyList :: [(String,[Int])]

metaclassDependencyList =
  concatMap (evalState parseMCLD) (lookupChunks "MCLD")

parseMCLD =
  do numEntries <- getUWord
     replicateM numEntries parseMCLD'

parseMCLD' =
  do state      <- get
     entryLen   <- getUWord
     entryName  <- getString1
     numPropIDs <- getUWord
     recordSize <- getUWord
     if recordSize /= 2 then
       error "Bad TADS3 file: unexpected property ID record size in MCLD block"
      else do
       propIDs    <- replicateM numPropIDs getUWord
       put state
       getBytes entryLen
       return (entryName,propIDs)


{-----------}


lookupMetaclass :: String -> Maybe (String,String,[String])
nameMetaclass :: Int -> String

lookupMetaclass = lookupMetaclass' intrinsicClasses

lookupMetaclass' [] id = Nothing

lookupMetaclass' (entry@(idPrefix,_,_):rest) id
  | idPrefix `isPrefixOf` id  = Just entry
  | otherwise                 = lookupMetaclass' rest id

nameMetaclass n =
  lookupMetaclassName (fst (metaclassDependencyList !! n))

lookupMetaclassName id =
  case lookupMetaclass id of
    Just (_,name,_) -> name
    Nothing         -> "{{{metaclass " ++ id ++ "}}}"


{-----------}

functionSetDependencyList :: [String]

functionSetDependencyList =
  concatMap (evalState parseFNSD) (lookupChunks "FNSD")

parseFNSD =
  do numEntries <- getUWord
     replicateM numEntries getString1

{-----------}

symdObjectNames,symdPropertyNames :: [(Int,String)]

symdObjectNames = [(val,name) | (5,val,name) <- symbolicNames]
symdPropertyNames = [(val.&.65535,name) | (6,val,name) <- symbolicNames]

symbolicNames =
  concatMap (evalState parseSYMD) (lookupChunks "SYMD")

parseSYMD =
  do numEntries <- getUWord
     replicateM numEntries parseSYMD'

parseSYMD' =
  do dataType <- getUByte
     val      <- getDword
     name     <- getString1
     return (dataType,val,name)

{-----------}

otherChunks :: [(String,[DataBlock])]

otherChunks =
  [(tag,blocks) | (tag,blocks) <- chunks, tag `notElem` knownTags]

knownTags = ["CPDF","CPPG","ENTP","OBJS","MCLD","FNSD","SYMD"]

{-----------}


stringAt :: Int -> String

stringAt n = evalState getString2 (dataFrom n)


{-----------}


intrinsicClasses =
 [
  ("root-object/03",	"Object",		["ofKind","getSuperclassList","propDefined","propType","getPropList","getPropParams","isClass","propInherited","isTransient"]),
  ("intrinsic-class/03","IntrinsicClass",	[]),
  ("int-class-mod/03",	"IntrinsicClassModifier",[]),
  ("collection/03",	"Collection",		["createIterator","createLiveIterator"]),
  ("iterator/03",	"Iterator",		["getNext","isNextAvailable","resetIterator","getCurKey","getCurVal"]),
  ("indexed-iterator/03","IndexedIterator",	[]),
  ("anon-func-ptr",	"AnonFuncPtr",		[]),
  ("tads-object/03",	"TadsObject",		["createInstance","createClone","createTransientInstance"]),
  ("string/03",		"String",		["length","substr","toUpper","toLower","find","toUnicode","htmlify","startsWith","endsWith","mapToByteArray","findReplace"]),
  ("list/03",		"List",			["subset","mapAll","length","sublist","intersect","indexOf","car","cdr","indexWhich","forEach","valWhich","lastIndexOf","lastIndexWhich","lastValWhich","countOf","countWhich","getUnique","appendUnique","append","sort","prepend","insertAt","removeElementAt","removeRange","forEachAssoc"]),
  ("regex-pattern/03",	"RexPattern",		["getPatternString"]),

  ("bignumber/03",	"BigNumber",		["formatString","equalRound","getPrecision","setPrecision","getFraction","getWhole","roundToDecimal","getAbs","getCeil","getFloor","getScale","scaleTen","negate","copySignFrom","isNegative","divideBy","sine","cosine","tangent","degreesToRadians","radiansToDegrees","arcsine","arccosine","arctangent","sqrt","logE","expE","log10","raiseToPower","sinh","cosh","tanh","getPi","getE"]),
  ("bytearray/03",	"ByteArray",		["length","subarray","copyFrom","fillValue","mapToString"]),
  ("character-set/03",	"CharacterSet",		["getName","isMappingKnown","isMappable","isRoundTripMappable"]),
  ("dictionary2/03",	"Dictionary",		["setComparator","findWord","addWord","removeWord","isWordDefined","forEachWord"]),
  ("file/03",		"File",			["openTextFile","openDataFile","openRawFile","getCharacterSet","setCharacterSet","closeFile","readFile","writeFile","readBytes","writeBytes","getPos","setPos","setPosEnd","openTextResource","openRawResource"]),
  ("grammar-production/03","GrammarProd",	["parseTokens"]),
  ("lookuptable/03",	"LookupTable",		["isKeyPresent","removeElement","applyAll","forEach","getBucketCount","getEntryCount","forEachAssoc","keysToList","valsToList"]),
  ("lookuptable-iterator/03","LookupTableIterator",[]),
  ("string-comparator/03","StringComparator",	["calcHash","matchValues"]),
  ("vector/03",		"Vector",		["toList","length","copyFrom","fillValue","subset","applyAll","indexWhich","forEach","forEachAssoc","mapAll","indexOf","valWhich","lastIndexOf","lastIndexWhich","lastValWhich","countOf","countWhich","getUnique","appendUnique","sort","setLength","insertAt","removeElementAt","removeRange","append","prepend","appendAll","removeElement"]),
  ("weakreflookuptable/03","WeakRefLookupTable",[])
 ]


{-----------}


negateExpr :: Expr -> Expr


negateExpr (Unary "!" exp) = exp

negateExpr (Binary left (op,prec) right) | isJust x =
  Binary left (fromJust x, prec) right
  where x = op `lookup` binopNegatives

negateExpr (Binary left op right) | op == _and_ || op == _or_ =
  Binary (negateExpr left) (if op == _and_ then _or_ else _and_) (negateExpr right)

negateExpr (IsIn left b right) = IsIn left (not b) right

negateExpr exp = Unary "!" exp


binopNegatives =
 [(" == "," != "),(" != "," == "),
  (" <= "," > "),(" > "," <= "),
  (" >= "," < "),(" < "," >= ")]


isBoolean (Unary "!" _)       = True
isBoolean (Binary _ (op,_) _)
  | op `elem` booleanBinops   = True
isBoolean (IfThenExpr a b c)  = isBoolean b && isBoolean c
isBoolean (IsIn _ _ _)        = True
isBoolean _                   = False

booleanBinops = [" == "," != "," < "," > "," <= "," >= "," && "," || "]


_or_,_and_ :: (String,Int)

_or_  = (" || ",4)
_and_ = (" && ",5)
