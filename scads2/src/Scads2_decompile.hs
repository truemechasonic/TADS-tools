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


module Scads2_decompile (decompile) where


import Scads2_types
import Scads2_vmmisc

import Data.List (sort)


decompile = doToDeath decompile3 . doToDeath decompile'

doToDeath f code =
  findFixedPoint (iterate (f . updateLabels) code)
    where findFixedPoint (x:y:z)
            | x == y    = x
            | otherwise = findFixedPoint (y:z)



decompile' :: [Instr] -> [Instr]


decompile' (PUSH v : PUSH w : BINARY op : rest) =
  decompile' (PUSH (Binary v op w) : rest)

decompile' (PUSH v : PUSH w : ASSIGN op : rest) =
  decompile' (PUSH (Assign w op v) : rest)

decompile' (PUSH v : PUSH w : INDEX : rest) =
  decompile' (PUSH (Index v w) : rest)

decompile' (PUSH v : PUSH w : SWAP : rest) =
  decompile' (PUSH w : PUSH v : rest)


decompile' (PUSH v : UNARY op : rest) =
  decompile' (PUSH (Unary op v) : rest)

decompile' (PUSH v : NOT : rest) =
  decompile' (PUSH (safeNegateExpr v) : rest)

decompile' (PUSH v : INCDEC True op : rest) =
  decompile' (PUSH (Unary op v) : rest)

decompile' (PUSH v : INCDEC False op : rest) =
  decompile' (PUSH (Postfix v op) : rest)


decompile' (PUSH func : CALL argc : rest) =
  decompile' (PartialCall (PartialFunc func) argc [] : rest)

decompile' (PUSH obj : PUSH prop : CALLPROP prefix argc : rest) =
  decompile' (PartialCall (PartialProp prefix obj prop) argc [] : rest)

decompile' (CONS argc : rest) =
  decompile' (PartialCall PartialCons argc [] : rest)

decompile' (PUSH arg : PartialCall x argc args : rest) | argc > 0 =
  decompile' (PartialCall x (argc-1) (arg:args) : rest)

decompile' (PartialCall (PartialFunc func) 0 args : rest) =
  decompile' (PUSH (Call func (reverse args)) : rest)

decompile' (PartialCall (PartialProp prefix obj prop) 0 args : rest) =
  decompile' (PUSH (CallProp prefix obj prop (reverse args)) : rest)

decompile' (PartialCall PartialCons 0 args : rest) =
  decompile' (PUSH (List (reverse args)) : rest)


decompile' [i@(PUSH (Quote (SS _))), RETVAL] = [i]
decompile' [i@(PUSH (Call (Quote (BUILTIN 0)) [_, Quote (C "nil")])), RETVAL] = [i]

decompile' (PUSH a : JT l : PUSH b : JMP m : LABEL l' Single : PUSH c : rest@(LABEL m' _ : _)) | l == l' && m == m'  =
  decompile' (PUSH (IfThenExpr (negateExpr a) b c) : rest)

decompile' (PUSH v : JS b l : PUSH w : rest@(LABEL l' _ : _)) | l == l'  =
  decompile' (PUSH (Binary v (if b then _or_ else _and_) w) : rest)


decompile' (NOP : rest) =
  decompile' rest


decompile' (BLOCK t body : rest) =
  BLOCK t (decompile' body) : decompile' rest

decompile' (THENELSE a b : rest) =
  THENELSE (decompile' a) (decompile' b) : decompile' rest


decompile' (instr : rest) =
  instr : decompile' rest

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

decompile3 (THENELSE a b : rest) =
  THENELSE (decompile3 a) (decompile3 b) : decompile3 rest

decompile3 (BLOCK t body : rest) =
  BLOCK t (decompile3 body) : decompile3 rest

decompile3 (instr : rest) =
  instr : decompile3 rest

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


safeNegateExpr x@(Unary "!" y) =
  if isBoolean y then y else (Unary "!" x)

safeNegateExpr x =
  if isBoolean x then negateExpr x else (Unary "!" x)

isBoolean (Unary "!" _)       = True
isBoolean (Binary _ (op,_) _)
  | op `elem` booleanBinops   = True
isBoolean (IfThenExpr a b c)  = isBoolean b && isBoolean c
isBoolean _                   = False

booleanBinops = [" == "," != "," < "," > "," <= "," >= "," && "," || "]


updateLabels code = updateLabels' (sort (getJumpTargets code)) code

updateLabels' jumps [] = []

updateLabels' jumps (LABEL ad oldType : rest) =
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
