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


module Main () where

import Scads3_types
import Scads3_disasm
import Scads3_decompile
import Scads3_objs
import Scads3_vmmisc
import Scads3_read
import Scads3_print
import Scads3_util

import Array (Array,array,(!))
import Char (chr,isAlphaNum)
import List (sort,sortBy)
import Maybe (isJust,fromJust,fromMaybe,mapMaybe)

import Data.Bits (testBit,(.&.))

import Control.Monad (replicateM)
import Control.Monad.State (State,get,put,evalState)


{------------------------- misc -------------------------}


uglyPrint (TadsCode _ _ instrs _ _) = mapM_ print instrs


{------------------------- (top-level) functions -------------------------}

allTopLevelFunctions =
  let known = uniq (sort (entpMainAddr : concatMap getTadsObjectFuncRefs
                                                   (map snd tadsObjects)))
  in findFunctions [] known


findFunctions old [] = []

findFunctions old new =
  let newFuncs = map tadsCodeAt new
      newRefs  = concatMap getTadsCodeFuncRefs newFuncs
      old'     = setMerge old new
      new'     = setDiff (uniq (sort newRefs)) old'
  in
    newFuncs ++ findFunctions old' new'
  where
    removeDupes [] _ = []
    removeDupes r [] = r
    removeDupes (r:rs) c@(TadsCode ad _ _ _ _:c') =
      case r `compare` ad of
        LT -> r : removeDupes rs c
        EQ -> removeDupes rs c'
        GT -> removeDupes (r:rs) c'


{-----------}


decompileObject (x, TadsObject (persist,super,props)) =
  (x, TadsObject (persist, super, map (onSnd decompileValue) props))
decompileObject x = x

decompileValue (CODE tc) = CODE (decompile tc)
decompileValue x = x


{------------------------- main -------------------------}

main = do
  mapM_ (putStrLn . ppObject . decompileObject) objects
  mapM_ (putStrLn . ppTadsFunc . decompile) (sort allTopLevelFunctions)


-- FIXME: Constructor -> construct, Destructor -> finalize
