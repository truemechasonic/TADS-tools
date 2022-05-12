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


module Scads3_print (
	ppObject,ppTadsFunc
) where


import Scads3_types
import Scads3_util
import Scads3_objs
import Scads3_read
import Scads3_vmmisc
import Scads3_disasm (getDataHolder)

import Data.Char (isAlphaNum,toLower)
import Data.List (sort)
import Data.Bits (testBit,(.&.))
import Control.Monad (replicateM)
import Control.Monad.State (evalState)


nameLabel l = "label" ++ show l

nameObject x = nameObj x (chooseTadsObjectName x)

chooseTadsObjectName n =
  case n `lookup` tadsObjectNames of
    Just name -> name
    Nothing   -> "unknownObj" ++ show n


nameArg n = "arg" ++ show n


{-----------}


tadsObjectNames = map guessObjName tadsObjects

guessObjName (id,(isClass,_,props)) =
  (id, (if isClass then "class" else "obj") ++ show id ++ findText props)

findText []          = ""
findText ((_,S s):_) = '_' : take 20 (map makeIdent s)
findText (_:rest)    = findText rest

makeIdent x = if isAlphaNum x then x else '_'


{-----------}


ppObject :: (Int,Object) -> String

ppObject obj =
  renders (ppObject' obj) "\n"

ppObject' (id,TadsObject (isClass,superclasses,properties)) =
  text (if isClass then "class " else "") <> text (nameObject id)
   <> ppSuperclasses superclasses
   <> block (map ppProperty properties) <> text ";"

ppObject' (id,x@(GrammarProd _)) = text (show x)

ppObject' (id,OtherObject (cls,_,blk)) =
  case evalState (ppOther cls) blk of
    Nothing -> text "/* " <> text (nameObject id) <> text " " <> text (show blk) <> text " */"
    Just x  -> text (nameObject id) <> text " = " <> x


{-----------}


ppOther "RexPattern" =
  do v <- getDataHolder		-- FIXME?
     return $ Just (text "static new RexPattern(" <> value v <> text ");")

ppOther "Vector" =
  do space  <- getUWord
     size   <- getUWord
     values <- replicateM size getDataHolder
     return $ Just (text "static new Vector" <> ppArgList [Quote (I space), Quote (LIST values)] <> text ";")

ppOther x = return Nothing


ppProperty (id,CODE tadsCode) = ppPropertyFunc id tadsCode

ppProperty (id,val) = text (nameProp id) <> text " = " <> value val


ppPropertyFunc id tc =
  text (nameProp id) <> ppTadsCode tc

ppParamList tc =
  let argCount = tcArgCount tc
      a = map (text . nameArg) (take (argCount .&. 127) [0..])
      b = if testBit argCount 7 then [text "[args]"] else []
  in text "(" <> join "," (a++b) <> text ")"


{-----------}


ppSuperclasses [] =
  text ": object"
ppSuperclasses superclasses =
  text ": " <> join ", " (map (text . nameObject) superclasses)


{-----------}


ppTadsFunc :: TadsCode -> String


ppTadsFunc tc =
  renders (ppTadsFunc' tc) "\n"

ppTadsFunc' tc =
  text (nameFunc (tcAddr tc)) <> ppTadsCode tc


{-----------}

ppTadsCode tc =
  ppParamList tc <> text " {"
   <> block (ppInstrs (tcCode tc)) <> text "}"


ppInstrs [] = []

ppInstrs (LABEL _ Phantom : rest) = ppInstrs rest

ppInstrs (LABEL l _ : rest) =
  outdent (text (nameLabel l) <> text ":") : ppInstrs rest

ppInstrs (CASE (Just val) : rest) =
  outdent (text "case " <> ppExpr 0 (Quote val) <> text ":") : ppInstrs rest

ppInstrs (CASE Nothing : rest) =
  outdent (text "default:") : ppInstrs rest

ppInstrs (NOP : rest)  = ppInstrs rest

ppInstrs (PUSH e : DISC : rest) =
  (ppExpr 0 e <> text ";") : ppInstrs rest

ppInstrs (PUSH e : RETVAL : rest) =
  (text "return " <> ppExpr 0 e <> text ";") : ppInstrs rest

ppInstrs (PUSH e : THROW : rest) =
  (text "throw " <> ppExpr 0 e <> text ";") : ppInstrs rest

ppInstrs (PUSH v : SAYVAL : rest') =
  let (says,rest) = onFst (v:) (getSays rest') in
    foldr (<>) (text "\";") (text "\"" : map ppSay says) : ppInstrs rest
  where
    getSays (PUSH e : SAYVAL : rest) = onFst (e:) (getSays rest)
    getSays rest = ([],rest)

ppInstrs (PUSH e : JT l : rest) =
  (text "if (" <> ppExpr 0 e <> text ") " <> ppInstrsGoto l) : ppInstrs rest

ppInstrs (PUSH e : THENELSE yes no : rest) =
  ppInstrsIf (negateExpr e) yes no : ppInstrs rest

ppInstrs (JMP l : rest)  = ppInstrsGoto l : ppInstrs rest

ppInstrs (BLOCK InfiniteLoop body : rest) =
  (text "for (;;) {" <> block (ppInstrs body) <> text "}") : ppInstrs rest

ppInstrs (PUSH expr : BLOCK (Switch cases) body : rest) =
  (text "switch (" <> ppExpr 0 expr <> text ") {"
    <> block (ppInstrs body) <> text "}") : ppInstrs rest

ppInstrs (BLOCK Try body : rest) =
  (text "try {" <> block (ppInstrs body) <> text "}") : ppInstrs rest

ppInstrs (BLOCK (Catch n) body : rest) =
  let typeName = if n == 0 then [] else [text (nameObject n)]
      (argName,body') = case body of
                          PUSH (Quote v) : SET : rest -> ([value v], rest)
                          _                           -> ([], body)
  in (text "catch (" <> join " " (typeName++argName) <> text ") {"
       <> block (ppInstrs body') <> text "}") : ppInstrs rest

ppInstrs (BLOCK Finally body : rest) =
  (text "finally {" <> block (ppInstrs body) <> text "}") : ppInstrs rest

ppInstrs (PUSH x : rest) = (text "{{{PUSH " <> ppExpr 0 x <> text "}}}") : ppInstrs rest

-- FIXME: should get rid of these in the decompiler
ppInstrs (LJSR _ : rest) = ppInstrs rest
ppInstrs (LRET _ : rest) = ppInstrs rest

ppInstrs (x : rest) = (text "{{{" <> text (show x) <> text "}}}") : ppInstrs rest


ppInstrsGoto label = text "goto " <> text (nameLabel label) <> text ";"


ppInstrsIf pred cons alt =
  text "if (" <> ppExpr 0 pred <> text ") {"
   <> block (ppInstrs cons) <> text "}" <> ppInstrsElse alt

ppInstrsElse [] = text ""

ppInstrsElse code =
  text " else " <> maybeBlock (ppInstrs code)
  where maybeBlock =
          case code of
            [PUSH _, THENELSE _ _] -> \[x] -> x
            _ -> \x -> text "{" <> block x <> text "}"


ppSay (Quote (S s)) = qtext s '"'

-- The precedence=11 here avoids problems with stuff like
-- "The value is <<(x >> 7)>>". But it also causes some other
-- expressions to be parenthesized unnecessarily.
ppSay e = text "<<" <> ppExpr 11 e <> text ">>"


-- The first argument is an "ambient precedence": if an operator has
-- precedence <= this value, it will be parenthesized.

ppExpr prec (Unary op expr) =
  parenIf (16 <= prec) (text op <> ppExpr 15 expr)

ppExpr prec (Postfix expr op) =
  parenIf (14 <= prec) (ppExpr 13 expr <> text op)

ppExpr prec (Binary expr1 (op,opPrec) expr2) =
  let left  = ppExpr (opPrec-1) expr1
      right = ppExpr opPrec expr2
  in
    parenIf (opPrec <= prec) (left <> text op <> right)

ppExpr prec (IsIn expr b exprLst) =
  let left     = ppExpr 8 expr
      op       = if b then " is in " else " not in "
      rightLst = map (ppExpr 2) exprLst
      right    = text "(" <> join ", " rightLst <> text ")"
  in
    parenIf (9 <= prec) (left <> text op <> right)

ppExpr prec (IfThenExpr cond yes no) =
  parenIf (3 <= prec) (ppExpr 3 cond <> text " ? " <> ppExpr 2 yes <> text " : " <> ppExpr 2 no)

ppExpr prec (Index expr1 expr2) =
  parenIf (17 <= prec) (ppExpr 16 expr1 <> text "[" <> ppExpr 0 expr2 <> text "]")

ppExpr prec (Call func args) =
  parenIf (15 <= prec) (ppExpr 14 func <> ppArgList args)

ppExpr prec (CallProp p obj prop []) =
  parenIf (17 <= prec) (text p <> ppExpr 16 obj <> text "." <> propRHS prop)

ppExpr prec (CallProp p obj prop args) =
  parenIf (17 <= prec) (text p <> ppExpr 16 obj <> text "." <> propRHS prop <> ppArgList args)

ppExpr prec (Quote v) = value v

ppExpr prec (Assign dst src) =
  parenIf (1 <= prec) (ppExpr 1 dst <> text " = " <> ppExpr 0 src)

-- Not sure what should be done precedence-wise here
ppExpr prec (LstPar e) = ppExpr 0 e <> text "..."

ppExpr prec (AnonFunc tc) =
  text "new function" <> ppTadsCode tc

-- ppExpr _ e = text ("{{{" ++ show e ++ "}}}")


ppValue (C x) = text x
ppValue (I x) = text (show x)
ppValue (S x) = text "'" <> qtext x '\'' <> text "'"
ppValue (SS x) = text "\"" <> qtext x '"' <> text "\""
ppValue (CODE x) = text (show x)
ppValue (LIST x) = text "[" <> join ", " (map ppValue x) <> text "]"
ppValue (OBJ x) = text (nameObject x)
ppValue (PROP x) = text "&" <> text (nameProp x)
ppValue (FUNC x) = text (nameFunc x)
ppValue (BUILTIN x) = text (nameBuiltin x)
ppValue (ENUM x) = text (nameEnum x)
ppValue (Local x) = text "local" <> text (show x)	-- FIXME?
ppValue (Arg x) = text "arg" <> text (show x)		-- FIXME?
ppValue (SharedLocal x) = text "sharedlocal" <> text (show x)
ppValue (NEW perm mc) =
  text op <> text (nameMetaclass mc)
    where op = if perm then "new " else "new transient "


propRHS (Quote (PROP n)) = text (nameProp n)
propRHS x = text "(" <> ppExpr 0 x <> text ")"


ppArgList args = text "(" <> join ", " (map (ppExpr 2) args) <> text ")"


ppString s term =
  foldr (.) id (map helper s) where
    helper c = maybe (c:) (\x -> ('\\':).(x:)) (c `lookup` escapes)
    escapes = (term,term) :
              [('\x0F','^'),('\x0E','v'),('\x0B','b'),('\x15',' '),
               ('\x0A','n'),('\x09','t'),('\\','\\')]

{------------------------- intrinsic classes -------------------------}


intrinsicFunctionSets =
 [
  ("t3vm/01",		["t3RunGC","t3SetSay","t3GetVMVsn","t3GetVMID","t3GetVMBanner","t3GetVMPreinitMode",
			 "t3DebugTrace","t3GetGlobalSymbols","t3AllocProp","t3GetStackTrace"]),
  ("tads-gen/03",	["dataType","getArg","firstObj","nextObj","randomize","rand",
			 "toString","toInteger","getTime","rexMatch","rexSearch","rexGroup",
			 "rexReplace","savepoint","undo","saveGame","restoreGame","restartGame",
			 "max","min","makeString","getFuncParams"]),
  ("tads-io/03",	["tadsSay","setLogFile","clearScreen","morePrompt","inputLine",
			 "inputKey","inputEvent","inputDialog","inputFile","timeDelay",
			 "systemInfo","statusMode","statusRight","resExists","setScriptFile",
			 "getLocalCharSet","flushOutput","inputLineTimeout","inputLineCancel",
			 "bannerCreate","bannerDelete","bannerClear","bannerSay","bannerFlush",
			 "bannerSizeToContents","bannerGoTo","bannerSetTextColor",
			 "bannerSetScreenColor","bannerGetInfo","bannerSetSize",
			 "logConsoleCreate","logConsoleClose","logConsoleSay"]),
  ("t3vmTEST/010000",	["t3test_get_obj_id","t3test_get_obj_gc_state","t3test_get_charcode"])
 ]


propertyNamesFromIntrinsicClasses =
  uniq (sort (concatMap getProps metaclassDependencyList))
  where getProps (classname,propNumbers) = zip propNumbers methods
          where methods = lookupMetaclassMethods classname


lookupMetaclassMethods id =
  case lookupMetaclass id of
    Just (_,_,methods) -> methods
    Nothing            -> []


{------------------------- intrinsic functions -------------------------}


intrinsicNames =
  map getIntrinsicNames functionSetDependencyList

getIntrinsicNames name =
  let known = getKnownIntrinsicNames name in
    known ++ map (\n -> "{{{unknown intrinsic " ++ name ++ " #" ++ shows n "}}}") [length known..]

getKnownIntrinsicNames name =
  helper intrinsicFunctionSets where
    helper [] = []
    helper ((namePrefix,funcs):rest)
      | namePrefix == take (length namePrefix) name  = funcs
      | otherwise  = helper rest


{--------------------------------------------------------------------------------}


nameObj n defaultName =
  case n `lookup` objsList of
    Nothing                 -> "unknownObj" ++ show n
    Just ("TadsObject",_,_) -> defaultName
    Just (mc,_,_)           -> map toLower mc ++ show n

nameProp n =
  case n `lookup` symdPropertyNames of
    Just name -> name
    Nothing ->
      case n `lookup` propertyNamesFromIntrinsicClasses of
        Just name -> name
        Nothing   -> "prop" ++ show n

nameFunc n =
  if n == entpMainAddr then "_main" else "fun" ++ show n

nameEnum n = "enum" ++ show n

nameBuiltin (set,ind) = intrinsicNames !! set !! ind


{--------------------------------------------------------------------------------}

parenIf :: Bool -> Doc -> Doc
parenIf True x  = text "(" <> x <> text ")"
parenIf False x = x

join :: String -> [Doc] -> Doc
join sep [] = text ""
join sep xs = foldr1 (\a b -> a <> text sep <> b) xs

{--------------------------------------------------------------------------------}


data Doc = PPSeq Doc Doc
         | PPText String
         | PPQText String Char
         | PPValue Value
         | PPCodeBlock [Doc]
         | PPOutdent Doc

infixr 5 <>

a <> b = PPSeq a b

text  = PPText
qtext = PPQText
value = PPValue
block = PPCodeBlock
outdent = PPOutdent

renders :: Doc -> ShowS


renders = renders' 0

renders' i (PPSeq a b) = renders' i a . renders' i b

renders' i (PPText t)  = (t ++)

renders' i (PPQText t q)  = ppString t q

renders' i (PPValue v) = renders' i (ppValue v)

renders' i (PPCodeBlock []) = (showChar ' ')

renders' i (PPCodeBlock xs) =
  showChar '\n' . foldr combineLines (indent i) renderedLines
  where renderedLines = map (rendersIndent (i+2)) xs
        combineLines x y = x . showChar '\n' . y

indent = (indentCache !!)
indentCache = iterate (showString "  " .) id

rendersIndent i (PPOutdent x) = rendersIndent (i-1) x
rendersIndent i (PPText "")   = id
rendersIndent i doc           = indent i . renders' i doc
