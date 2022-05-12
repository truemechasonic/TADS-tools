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


module Scads3_decompile (decompile) where


import Scads3_types
import Scads3_disasm
import Scads3_vmmisc
import Scads3_util

import Data.List (sort,sortBy)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Debug.Trace (trace)


decompile :: TadsCode -> TadsCode


decompile tc =
  tc { tcCode = decompileAnonFuncs $
                doToDeath decompile3 $
                doToDeath decompile' $
                fixSwitches $
                structureExceptions (tcExceptionBlocks tc) (tcCode tc) }


doToDeath f code =
  findFixedPoint (iterate (f . updateLabels) code)
    where findFixedPoint (x:y:z)
            | x == y    = x
            | otherwise = findFixedPoint (y:z)


decompile' :: [Instr] -> [Instr]


decompile' (BLOCK (Switch []) body : rest) =
  BLOCK (Switch []) (decompile' body) : decompile' rest


decompile' (PUSH x : PUSH y : SWAP : rest) =
  decompile' (PUSH y : PUSH x : rest)

decompile' (PUSH v : BOOLIZE : rest) =
  decompile' (PUSH v' : rest)
    where v' = if isBoolean v then v else Unary "!" (negateExpr v)

decompile' (PUSH v : UNARY op : rest) =
  decompile' (PUSH (Unary op v) : rest)

decompile' (PUSH v : PUSH w : BINARY op : rest) =
  decompile' (PUSH (Binary v op w) : rest)

decompile' (PUSH v : JT k : PUSH w : rest@(JT m : _)) | k == m =
  decompile' (PUSH (Binary v _or_ w) : rest)

decompile' (PUSH v : JT k : PUSH w : rest@(JT m : LABEL ad2 _ : _)) | k == ad2 =
  decompile' (PUSH (Binary (negateExpr v) _and_ w) : rest)

decompile' (PUSH v : JT k : PUSH w : rest@(JS b m : LABEL ad2 _ : _)) | k == ad2 =
  decompile' (PUSH (Binary (negateExpr v) _and_ w) : rest)

decompile' (PUSH v : JS True k : PUSH w : rest@(LABEL ad2 _ : _)) | k == ad2 =
  decompile' (PUSH (Binary v _or_ w) : rest)

decompile' (PUSH v : JS False k : PUSH w : rest@(LABEL ad2 _ : _)) | k == ad2 =
  decompile' (PUSH (Binary v _and_ w) : rest)

decompile' (PUSH v : JT k : PUSH x : JMP m : LABEL ad2 Single : PUSH y : rest@(LABEL ad3 _ : _)) | k == ad2 && m == ad3 =
  decompile' (PUSH (IfThenExpr (negateExpr v) x y) : rest)

decompile' (PUSH (IfThenExpr cond (Quote (C x)) (Quote (C y))) : rest)
  | isBoolean cond && x == "true" && y == "nil"  = decompile' (PUSH cond : rest)
  | isBoolean cond && x == "nil" && y == "true"  = decompile' (PUSH (negateExpr cond) : rest)

decompile' (PUSH (Quote (I argc)) : PUSH val : MAKELSTPAR : rest) =
  decompile' (PUSH (LstPar val) : PUSH (Quote (I (argc+1))) : rest)

decompile' (PUSH lst : PUSH i : INDEX : rest) =
  decompile' (PUSH (Index lst i) : rest)

decompile' (PUSH lst : PUSH i : SETIND : PUSH lst' : rest@(SET : _)) | lst == lst' =
  decompile' (PUSH (Index lst i) : rest)


decompile' (PUSH src : PUSH dst : SET : rest) =
  decompile' (PUSH (Assign dst src) : DISC : rest)

decompile' (PUSH src : DUP : PUSH dst : SET : rest) =
  decompile' (PUSH (Assign dst src) : rest)

-- FIXME: this only happens in inline functions; need to handle them properly
decompile' (a@(PUSH _) : b@DUP : c@(PUSH _) : d@INDEX : e@(PUSH _) : f@SET : g@(PUSH _) : h@SET : rest) =
  decompile' (a:b:g:h:c:d:e:f:rest)


-- There's at least one case in which the TADS compiler generates an
-- INC instruction for a value on the stack which is a compile-time
-- constant. I have to handle this because it then calls VARARGC.
decompile' (PUSH (Binary (Quote (I x)) (" + ",_) (Quote (I y))) : rest) =
  decompile' (PUSH (Quote (I (x+y))) : rest)


decompile' (PUSH (Quote (I argc)) : VARARGC : rest) =
  helper id rest where
    helper sofar (CALL _ : rest) = decompile' (sofar (CALL argc : rest))
    helper sofar (CALLPROP p _ : rest) = decompile' (sofar (CALLPROP p argc : rest))
    helper sofar (PartialCall t _ a : rest) = decompile' (sofar (PartialCall t argc a : rest))
    helper sofar (x : rest) = helper (sofar . (x:)) rest
    helper _ _ = error "Bad TADS3 file: VARARGC without matching call instr"


decompile' (PUSH (Quote (I argc)) : CALL (-1) : rest) =
  decompile' (CALL argc : rest)

decompile' (PUSH (Quote (I argc)) : CALLPROP p (-1) : rest) =
  decompile' (CALLPROP p argc : rest)


decompile' (PUSH func : CALL argc : rest) | argc >= 0 =
  decompile' (PartialCall (PartialFunc func) argc [] : rest)

decompile' (PUSH obj : PUSH prop : CALLPROP p argc : rest) | argc >= 0 =
  decompile' (PartialCall (PartialProp p obj prop) argc [] : rest)


decompile' (PUSH obj1 : PUSH (Assign obj2 (Binary obj3 (op,_) (Quote (I 1)))) : DISC : rest) | obj1 == obj2 && obj2 == obj3 && op `elem` [" + "," - "] =
  decompile' (PUSH (Postfix obj1 (if op == " + " then "++" else "--")) : rest)

decompile' (PUSH obj1 : DUP : rest@(PUSH (Quote (I 1)) : BINARY (_,12) : PUSH obj2 : SET : _)) | obj1 == obj2 =
  decompile' (PUSH obj1 : PUSH obj1 : rest)


decompile' (PartialCall (PartialProp p obj prop) 0 args : PUSHR0 : rest) =
  decompile' (PUSH (CallProp p obj prop (reverse args)) : rest)

decompile' (PartialCall (PartialProp p obj prop) 0 args : rest) =
  decompile' (PUSH (CallProp p obj prop (reverse args)) : DISC : rest)

decompile' (PartialCall (PartialFunc func) 0 args : PUSHR0 : rest) =
  decompile' (PUSH (Call func (reverse args)) : rest)

decompile' (PartialCall (PartialFunc func) 0 args : rest) =
  decompile' (PUSH (Call func (reverse args)) : DISC : rest)

decompile' (PUSH (Quote x) : DUP : rest@(PartialCall _ _ _ : _)) =
  decompile' (PUSH (Quote x) : PUSH (Quote x) : rest)


decompile' (NOP : rest) =
  decompile' rest


decompile' (first@(JIN b x l) : DISC : rest) =
  case findMatch (findJINTarget l) rest of
    Just (a,b) -> first : decompile' (a ++ b)
    Nothing    -> first : decompile' (DISC : rest)
  where findJINTarget l (i@(LABEL l' Single) : DISC : rest) | l == l'  =
          Just (i : rest)
        findJINTarget _ _ = Nothing

decompile' (PUSH val : JIN b x l : PUSH (Quote (C "nil")) : JMP m : LABEL l' Single : PUSH (Quote (C "true")) : rest@(LABEL m' _ : _)) | l==l' && m==m'  =
  decompile' (PUSH (IsIn val b x) : rest)

decompile' (PUSH val : JIN b x l : PUSH (Quote (C "true")) : JMP m : LABEL l' Single : PUSH (Quote (C "nil")) : rest@(LABEL m' _ : _)) | l==l' && m==m'  =
  decompile' (PUSH (IsIn val (not b) x) : rest)

decompile' (PUSH val : JIN b x l : PUSH (Quote (C "nil")) : RETVAL : LABEL l' Single : PUSH (Quote (C "true")) : RETVAL : rest) | l==l'  =
  decompile' (PUSH (IsIn val b x) : RETVAL : rest)

decompile' (PUSH val : JIN b x l : PUSH (Quote (C "true")) : RETVAL : LABEL l' Single : PUSH (Quote (C "nil")) : RETVAL : rest) | l==l'  =
  decompile' (PUSH (IsIn val (not b) x) : RETVAL : rest)


decompile' (DUP : PUSH v : BINARY (op,9) : JT label : rest) | op == (" == ") || op == " != " =
  decompile' (JIN (op == (" == ")) [v] label : rest)

decompile' (JIN b x l : JIN b' y l' : rest) | b==b' && l==l' =
  decompile' (JIN b (x++y) l : rest)


decompile' (PUSH arg : PartialCall t argc args : rest) | argc > 0 =
  decompile' (PartialCall t (argc-1) (arg:args) : rest)


decompile' (BLOCK (Catch 0)
                  (PUSH (Quote (Local loc)) : SET :
                   LJSR fin :
                   PUSH (Quote (Local loc')) : THROW :
                   LABEL fin' _ :
                   PUSH (Quote (Local _)) : SET :
                   restInside) : restOutside)
  | loc == loc' && fin == fin'  =
  BLOCK Finally (decompile' restInside) : decompile' restOutside


decompile' (BLOCK t body : rest) =
  BLOCK t (decompile' body) : decompile' rest


decompile' (RETVAL : JMP _ : rest) =
  decompile' (RETVAL : rest)

-- needed?
decompile' (RETVAL : RETVAL : rest) =
  decompile' (RETVAL : rest)


-- FIXME: see if this is necessary
-- decompile' (RETVAL : instr : rest) | not (isJumpTarget instr) =
--   decompile' (RETVAL : rest)
-- decompile' (stop@(JMP _) : instr : rest) | not (isJumpTarget instr) =
--   decompile' (stop : rest)


decompile' (x : xs) = x : decompile' xs

decompile' [] = []


decompile3 (first@(JT midLabel) : rest) =
  case findMatch (findIfThenElse midLabel) rest of
    Just (thenClause,(elseClause,(endLabel,rest'))) ->
      let exit = [LABEL endLabel Phantom] in
        THENELSE (thenClause ++ exit) (elseClause ++ exit) : decompile3 rest'
    Nothing ->
      case findMatch (findIfThen midLabel) rest of
        Just (thenClause,rest') ->
          let exit = [LABEL midLabel Phantom] in
            THENELSE (thenClause ++ exit) [] : decompile3 rest'
        Nothing -> first : decompile3 rest

decompile3 (BLOCK t body : rest) =
  BLOCK t (decompile3 body) : decompile3 rest

decompile3 (THENELSE yes no : rest) =
  THENELSE (decompile3 yes) (decompile3 no) : decompile3 rest

decompile3 (x : xs) = x : decompile3 xs

decompile3 [] = []


findIfThenElse l (JMP endLabel : LABEL l' Single : rest) | l == l'  =
  findMatch findElse rest where
    findElse all@(LABEL endLabel' _ : rest) | endLabel == endLabel' = Just (endLabel,all)
    findElse _ = Nothing

findIfThenElse _ _ = Nothing


findIfThen l all@(LABEL l' _ : _) | l == l'  = Just all
findIfThen _ _ = Nothing


findMatch = findMatch' id

findMatch' before f [] = Nothing
findMatch' before f (x:xs) =
  case f (x:xs) of
    Just result -> Just (before [],result)
    Nothing     -> findMatch' (before.(x:)) f xs


updateLabels code = updateLabels' (sort (getJumpTargets code)) code

updateLabels' jumps [] = []

updateLabels' jumps ((LABEL ad oldType) : rest) =
  let (jumpsHere,jumpsLater) = break (> ad) $ dropWhile (< ad) jumps
  in updateLabel ad oldType jumpsHere ++ updateLabels' jumpsLater rest

updateLabels' jumps (BLOCK t body : rest) =
  BLOCK t (updateLabels' jumps body) : updateLabels' jumps rest
updateLabels' jumps (THENELSE yes no : rest) =
  THENELSE (updateLabels' jumps yes) (updateLabels' jumps no) : updateLabels' jumps rest

updateLabels' jumps (x:xs) = x : updateLabels' jumps xs


updateLabel ad _       []  = []
updateLabel ad Phantom _   = [LABEL ad Phantom]
updateLabel ad _       [_] = [LABEL ad Single]
updateLabel ad _       _   = [LABEL ad Multi]


getJumpTargets = concatMap getJumpTargets'

getJumpTargets' (JMP  n)  = [n]
getJumpTargets' (JT   n)  = [n]
getJumpTargets' (JS _ n)  = [n]
getJumpTargets' (JIN _ _ n) = [n]
getJumpTargets' (LJSR n)  = [n]
getJumpTargets' (BLOCK _ body) = getJumpTargets body
getJumpTargets' (THENELSE yes no) = concatMap getJumpTargets' (yes ++ no)
getJumpTargets' _ = []


{------------}

decompileAnonFuncs orig@(PUSH (Assign (Quote (Local local)) (Call (Quote (NEW True vector)) [Quote (I len), Quote (I len')])) : DISC : rest) | len == len' && isVectorMetaclass vector =
  case decompileAnonFuncs' local rest of
    Nothing       -> orig
    Just (code,0) -> orig
    Just (code,_) -> code

decompileAnonFuncs x = x


-- returns Nothing if we can't handle something,
-- otherwise Just (newCode,numAnonFuncs)

decompileAnonFuncs' :: Int -> [Instr] -> Maybe ([Instr],Int)

decompileAnonFuncs' local code =
  mapDeanon (deanon local) code


deanon :: Int -> Instr -> Maybe (Instr,Int)

deanon l (PUSH expr) = liftDeanon PUSH (deanonExpr l expr)

-- fail these rather than handle nested expressions
deanon l (JIN _ _ _) = fail_ "1"
deanon l (PartialCall _ _ _) = fail_ "2"

deanon l x = return (x,0)


fail_ reason = {- trace reason $ -} fail ""


deanonExpr l (Unary op expr) =
  liftDeanon (Unary op) (deanonExpr l expr)

deanonExpr l (Binary left op right) =
  liftDeanon2 (flip Binary op) (deanonExpr l left) (deanonExpr l right)

deanonExpr l (Postfix expr op) =
  liftDeanon (flip Postfix op) (deanonExpr l expr)

deanonExpr l (IfThenExpr e f g) =
  liftDeanon3 IfThenExpr (deanonExpr l e) (deanonExpr l f) (deanonExpr l g)

-- FIXME: check what the compiler does with nested anonymous functions
deanonExpr l (Index (Quote (Local local)) (Quote (I n))) | local == l  =
  return (Quote (SharedLocal n), 0)

deanonExpr l (Index e f) =
  liftDeanon2 Index (deanonExpr l e) (deanonExpr l f)

deanonExpr l (IsIn left b right) =
  liftDeanon2 (flip IsIn b) (deanonExpr l left) (mapDeanon (deanonExpr l) right)

deanonExpr l (Call (Quote (NEW _ anonfunc)) args) | isAnonFuncMetaclass anonfunc =
  case args of
    [Quote (FUNC addr), Quote (Local local)] | local == l ->
      do tc <- decompileAnonFuncAt addr
         return (AnonFunc tc, 1)
    _ -> fail_ (show args)

deanonExpr l (Call f args) =
  liftDeanon2 Call (deanonExpr l f) (mapDeanon (deanonExpr l) args)

deanonExpr l (CallProp p obj prop args) =
  liftDeanon3 (CallProp p) (deanonExpr l obj) (deanonExpr l prop) (mapDeanon (deanonExpr l) args)

deanonExpr l (Quote (Local local)) | local == l =
  fail_ "4"

deanonExpr l x@(Quote _) = return (x,0)

deanonExpr l x@(Assign left right) =
  liftDeanon2 Assign (deanonExpr l left) (deanonExpr l right)

deanonExpr l x@(LstPar expr) =
  liftDeanon LstPar (deanonExpr l expr)


decompileAnonFuncAt addr =
  let tc = decompile (tadsCodeAt addr) in return tc
--    case tcCode tc of
--      PUSH (Assign (Quote (C "self"))


mapDeanon f list = sumUp (mapM f list)

sumUp :: Maybe [(a,Int)] -> Maybe ([a],Int)

sumUp x =
  do (a,b) <- liftM unzip x
     return (a,sum b)


liftDeanon :: (a -> b) -> Maybe (a,Int) -> Maybe (b,Int)

liftDeanon  f a =
  do (x,p) <- a
     return (f x,p)

liftDeanon2 :: (a -> b -> c) -> Maybe (a,Int) -> Maybe (b,Int) -> Maybe (c,Int)

liftDeanon2 f a b =
  do (x,p) <- a
     (y,q) <- b
     return (f x y, p + q)

liftDeanon3 f a b c =
  do (x,p) <- a
     (y,q) <- b
     (z,r) <- c
     return (f x y z, p + q + r)


isVectorMetaclass mc =
  nameMetaclass mc == "Vector"

isAnonFuncMetaclass mc =
  nameMetaclass mc == "AnonFuncPtr"


{------------}

fixSwitches :: [Instr] -> [Instr]

fixSwitches (BLOCK (Switch cases) [] : rest) =
  let defaultCase            = fst (last cases)
      (beforeDefaultCase,_)  = findLabel defaultCase rest
      jumpsFromBeforeDefault = getJumpTargets beforeDefaultCase
      jumpsBeyondDefault     = filter (> defaultCase) jumpsFromBeforeDefault
      assumedEndOfSwitchStmt = if null jumpsBeyondDefault
                                 then defaultCase
                                 else minimum jumpsBeyondDefault
      (body,rest')  = findLabel assumedEndOfSwitchStmt rest
  in
    BLOCK (Switch []) (fixSwitches (addCases cases body)) : fixSwitches rest'

fixSwitches (x:xs) = x:fixSwitches xs
fixSwitches [] = []

findLabel label code =
  fromMaybe (code,[]) $ findMatch matchLabel code
  where matchLabel all@(LABEL l _ : _) | l == label  = Just all
        matchLabel _                                 = Nothing

-- addCases (_:_:_) [] = error "Case label not found"

addCases cases (LABEL ad t : rest) =
  let (casesHere,casesLater) = break ((/= ad) . fst) cases
  in  LABEL ad t : map (CASE . snd) casesHere ++ addCases casesLater rest

addCases cases (x : xs) = x : addCases cases xs
addCases _ [] = []


structureExceptions :: [((Int,Int),[(Int,Int)])] -> [Instr] -> [Instr]

structureExceptions exceptions =
  structureExceptions' (sortBy reverseHandlerOrder exceptions)
    where reverseHandlerOrder (_,((_,a):_)) (_,((_,b):_)) =
            b `compare` a

structureExceptions' [] code = code

structureExceptions' (((protectFrom,protectTo),handlerInfo) : exs) code =
  let (beforeTry,tryAndAfterTry) = findLabel protectFrom code
      Just (try,(afterTry,handlerEnd)) = findMatch (tryBlockEnd (protectTo+1)) tryAndAfterTry
      (handlers,afterHandlers) = getCodeBlocks (tail (map snd handlerInfo) ++ [handlerEnd]) afterTry
      handlerBlocks = zipWith (\e c -> BLOCK (Catch e) c) (map fst handlerInfo) handlers
      (exsInside,exsOutside) = break (\x -> fst (fst x) < protectFrom) exs
  in  structureExceptions' exsOutside beforeTry
       ++ BLOCK Try (structureExceptions' exsInside try)
       : handlerBlocks ++ afterHandlers

tryBlockEnd l (JMP after : rest@(LABEL l' _ : _)) | l == l' && after >= l  =
  Just (rest,after)

tryBlockEnd l all@(LABEL l' _ : _) | l == l'  =
  Just (all,0)

tryBlockEnd l _ = Nothing

getCodeBlocks (end:ends) code =
  let (this,rest) = if end==0 then (code,[]) else findLabel end code
  in  onFst (this:) $ getCodeBlocks ends rest

getCodeBlocks [] code = ([],code)
