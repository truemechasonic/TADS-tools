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


module Scads2_print (ppAll) where


import Scads2_types
import Scads2_vmmisc

import Data.Char (isAlphaNum)
import Data.Bits (testBit)
import Data.Maybe (fromMaybe)


ppCode :: (?info :: Info) => [Instr] -> [String]

ppCode = ppInstrs

ppInstrs [] = []

ppInstrs (LABEL _ Phantom : rest) = ppInstrs rest
ppInstrs (LABEL ad _ : rest) = ('<' : nameLabel ad ++ ":") : ppInstrs rest

ppInstrs (CASE (Just val) : rest) =
  ("<case " ++ ppExpr 0 (Quote val) ++ ":") : ppInstrs rest

ppInstrs (CASE Nothing : rest) =
  "<default:" : ppInstrs rest

ppInstrs (CHKARGC _ : rest) = ppInstrs rest

ppInstrs (ENTER 0 : rest) = ppInstrs rest

ppInstrs (ENTER numLocals : rest) =
  let (locals,rest') = getLocals 1 rest
      getLocals n all@(_ : DISC : _) = (restOfLocals n,all)
      getLocals n all@(PUSH asn@(Assign (Quote (Local k)) " = " x) : rest) =
        case k `compare` n of
          EQ  -> onFst (asn:) (getLocals (n+1) rest)
          LT  -> (restOfLocals n,all)
          GT  -> onFst (Quote (Local n):) (getLocals (n+1) all)
      getLocals n rest = (restOfLocals n,rest)
      restOfLocals n = map (Quote . Local) [n..numLocals]
  in
    ("local " ++ join ", " (map (ppExpr 0) locals) ++ ";") : "" : ppInstrs rest'

ppInstrs (PASS v : rest) = ("pass " ++ nameProp v ++ ";") : ppInstrs rest

ppInstrs (RETURN : rest) = "return;" : ppInstrs rest

ppInstrs all@(PUSH (Quote (SS _)) : _) = ppSay all
ppInstrs all@(PUSH (Call (Quote (BUILTIN 0)) [_, Quote (C "nil")]) : _) = ppSay all

ppInstrs (PUSH e : DISC : rest)    = (ppExpr 0 e ++ ";") : ppInstrs rest
ppInstrs (PUSH e : RETVAL : rest)  = ("return " ++ ppExpr 0 e ++ ";") : ppInstrs rest

ppInstrs (PUSH e : JT l : rest)  = ("if (" ++ ppExpr 0 e ++ ") " ++ ppInstrsGoto l) : ppInstrs rest

ppInstrs (PUSH e : THENELSE yes no : rest) =
  ppInstrsIf (negateExpr e) yes no ++ ppInstrs rest

ppInstrs (JMP l : rest)  = ppInstrsGoto l : ppInstrs rest

ppInstrs (BLOCK InfiniteLoop body : rest) =
  "for (;;) {" : indentBlock (ppInstrs body) ++ "}" : ppInstrs rest

ppInstrs (PUSH expr : BLOCK (Switch cases) body : rest) =
  ("switch (" ++ ppExpr 0 expr ++ ") {")
   : indentBlock (ppInstrs body) ++ "}" : ppInstrs rest

-- Print a ? "text1" : "text2" as an if-then statement
ppInstrs (PUSH x@(IfThenExpr _ _ _) : rest) =
  ppInstrs (ifThenExprToInstr x ++ rest)

-- The TADS2 compiler often doesn't pop things from the stack,
-- so try to catch that case here
ppInstrs (PUSH x : rest) = (ppExpr 0 x ++ ";") : ppInstrs rest

ppInstrs (x : rest) = ("{{{" ++ show x ++ "}}}") : ppInstrs rest


ifThenExprToInstr (IfThenExpr a b c) =
  [PUSH (negateExpr a), THENELSE (ifThenExprToInstr b) (ifThenExprToInstr c)]
ifThenExprToInstr e = [PUSH e]


ppSay x =
  let (says,rest) = gatherSays x
  in  ('"' : concatMap ppSay' says ++ "\";") : ppInstrs rest

gatherSays (PUSH (Quote (SS msg)) : rest) =
  onFst (Left msg :) (gatherSays (maybeDropDISC rest))

gatherSays (PUSH (Call (Quote (BUILTIN 0)) [expr, Quote (C "nil")]) : rest) =
  onFst (Right expr :) (gatherSays (maybeDropDISC rest))

gatherSays rest = ([],rest)

ppSay' (Left msg)   = ppString '"' msg
ppSay' (Right expr) = "<<" ++ ppExpr 9 expr ++ ">>"

maybeDropDISC (DISC : rest) = rest
maybeDropDISC x = x


ppInstrsGoto label = "goto " ++ nameLabel label ++ ";"


ppInstrsIf pred cons alt =
  ["if (" ++ ppExpr 0 pred ++ ") {"]
   ++ indentBlock (ppInstrs cons)
   ++ ppInstrsElse alt

ppInstrsElse [] = ["}"]

ppInstrsElse (PUSH e : THENELSE yes no : rest) =
  ("} else " ++ head x) : tail x ++ ppInstrs rest
    where x = ppInstrsIf (negateExpr e) yes no

ppInstrsElse code =
  ["} else {"] ++ indentBlock (ppInstrs code) ++ ["}"]


-- The first argument is an "ambient precedence": if an operator has
-- precedence <= this value, it will be parenthesized.

ppExpr prec (Unary op expr) =
  parenIf (12 <= prec) (op ++ ppExpr 11 expr)

ppExpr prec (Postfix expr op) =
  parenIf (13 <= prec) (ppExpr 12 expr ++ op)

ppExpr prec (Binary expr1 (op,opPrec) expr2) =
  let left  = ppExpr (opPrec-1) expr1
      right = ppExpr opPrec expr2
  in
    parenIf (opPrec <= prec) (left ++ op ++ right)

ppExpr prec (IfThenExpr cond yes no) =
  parenIf (2 <= prec) (ppExpr 2 cond ++ " ? " ++ ppExpr 1 yes ++ " : " ++ ppExpr 1 no)

ppExpr prec (Index expr1 expr2) =
  parenIf (14 <= prec) (ppExpr 13 expr1 ++ "[" ++ ppExpr 0 expr2 ++ "]")

ppExpr prec (Call func args) =
  parenIf (14 <= prec) (callLHS func ++ ppArgList args)

ppExpr prec (CallProp p obj prop []) =
  parenIf (14 <= prec) (p ++ ppExpr 13 obj ++ "." ++ propRHS prop)

ppExpr prec (CallProp p obj prop args) =
  parenIf (14 <= prec) (p ++ ppExpr 13 obj ++ "." ++ propRHS prop ++ ppArgList args)


ppExpr prec (List exprs) = '[' : join ", " (map (ppExpr 0) exprs) ++ "]"


ppExpr prec (Quote v) = ppValue v


ppExpr prec (Assign dst op src) =
  parenIf (1 <= prec) (ppExpr 1 dst ++ op ++ ppExpr 0 src)

-- ppExpr _ e = "{{{" ++ show e ++ "}}}"


ppValue (C x) = x
ppValue (I x) = show x
ppValue (S x) = ppString' '\'' x
ppValue (SS x) = ppString' '"' x
ppValue (LIST x) = '[' : join ", " (map ppValue x) ++ "]"
ppValue (OBJ x) = nameObject x
ppValue (FUNC x) = '&' : nameObject x
ppValue (PROP x) = '&' : nameProp x
ppValue (BUILTIN x) = nameBuiltin x
ppValue (Local x) = nameLocal x


callLHS (Quote (FUNC n)) = nameObject n
callLHS x = ppExpr 13 x

propRHS (Quote (PROP n)) = nameProp n
propRHS x = '(' : ppExpr 0 x ++ ")"


ppString' term s = term : ppString term s ++ [term]

ppString term s =
  concatMap helper s where
    helper c | c == term  = ['\\',c]
             | otherwise  = [c]


ppArgList args = '(' : join ", " (map (ppExpr 2) args) ++ ")"


join sep [] = []
join sep xs = foldr1 (\a b -> a ++ sep ++ b) xs


indentBlock =
  map foo where foo ('<' : x) = ' ':' ':x
                foo "" = ""
                foo x = ' ':' ':' ':' ':x


parenIf True x  = "(" ++ x ++ ")"
parenIf False x = x


nameLabel l = "label" ++ show l

nameObject o = nameObject' ?info o

nameProp p = nameProp' ?info p

nameBuiltin b = builtins !! b

builtins =
 ["say", "car", "cdr", "length", "randomize", "rand",
  "substr", "cvtstr", "cvtnum", "upper", "lower",
  "caps", "find", "getarg", "datatype", "setdaemon",
  "setfuse", "setversion", "notify", "unnotify",
  "yorn", "remfuse", "remdaemon", "incturn", "quit",
  "save", "restore", "logging", "input", "setit",
  "askfile", "setscore", "firstobj", "nextobj",
  "isclass", "restart", "debugTrace", "undo", "defined",
  "proptype", "outhide", "runfuses", "rundaemons",
  "gettime", "getfuse", "intersect", "inputkey",
  "objwords", "addword", "delword", "getwords",
  "nocaps", "skipturn", "clearscreen", "firstsc",
  "verbinfo", "fopen", "fclose", "fwrite", "fread",
  "fseek", "fseekeof", "ftell", "outcapture",
  "systemInfo", "morePrompt", "parserSetMe",
  "parserGetMe", "reSearch", "reGetGroup", "inputevent",
  "timeDelay", "setOutputFilter", "execCommand",
  "parserGetObj", "parseNounList", "parserTokenize",
  "parserGetTokTypes", "parserDictLookup",
  "parserResolveObjects", "parserReplaceCommand",
  "exitobj", "inputdialog", "resourceExists"]

nameLocal l
  | l >= 0     = "local" ++ show l
  | otherwise  = "arg" ++ show (-l)


{-----------------------------------------------------------}


getVocab chunks =
  let vocab = concat [x | ChunkVOC x <- chunks]
      vocab' = filter (not.inherited) vocab where inherited (_,_,_,f) = testBit f 1
      vocab'' = map (\(s,prop,obj,_) -> (obj,(prop,s))) vocab'
  in
    map (onSnd gather) (gather (stableSortBy foo vocab''))
  where foo (a,(b,c)) (d,(e,f)) = (a,b) < (d,e)

gather :: Eq a => [(a,b)] -> [(a,[b])]

gather [] = []
gather xs@((a,_):_) =
  (a,map snd y) : gather z where (y,z) = break ((/= a).fst) xs


getObjectDesc req _ (n,_) | Just reqName <- lookup n req  = (n,reqName)

getObjectDesc _ _ (n,TadsFunction _) = (n,"fun" ++ show n)

getObjectDesc _ vocab (n,TadsObject isClass _ props) =
  let front = (if isClass then "class" else "obj") ++ show n
      back = case lookup 8 props of	-- sdesc
               Just (Val (SS desc)) -> desc
               _ ->
                 case lookup n vocab of
                   Just vprops ->
                     case lookup 3 vprops of	-- noun
                       Just words -> join " " words
                       Nothing -> ""
                   Nothing -> ""
  in (n,if null back then front else (front ++ '_' : makeIdent back))

getObjectDesc _ _ (n,_) = (n,"unknownObj" ++ show n)


makeIdent x =
  take 20 (map helper x) where helper c = if isAlphaNum c then c else '_'


getObjectDescs vocab chunks =
  let objs = concat [x | ChunkOBJ x <- chunks]
      req  = concat [x | ChunkREQ x <- chunks]
  in map (getObjectDesc req vocab) objs


getPropDescs chunks objectDescs =
  let objs  = concat [o | ChunkOBJ o <- chunks]
      props = concat [[(n,q) | q <- p] | (n,TadsObject _ _ p) <- objs]
      tpl2s = concat [[(n,u) | u <- t] | (n,(_,TPL2 t)) <- props]
      actionNames = map (("Action" ++).show) [0..]
  in zip [1..] stdProps ++ concat (zipWith helper tpl2s actionNames)
  where
    helper (n,(m,(verIoProp,ioProp,verDoProp,doProp),_)) actionName =
      let x = getFullActionName m $ getFullActionName n actionName in
        [(verIoProp, "verIo" ++ x),(ioProp, "io" ++ x),
         (verDoProp, "verDo" ++ x),(doProp, "do" ++ x)]
    getFullActionName n actionNamePrefix =
      case lookup n objectDescs of	-- sdesc
        Nothing   -> actionNamePrefix
        Just name -> actionNamePrefix ++ dropWhile (/= '_') name


stdProps =
 ["doAction", "verb", "noun", "adjective", "preposition",
  "article", "plural", "sdesc", "thedesc", "doDefault",
  "ioDefault", "ioAction", "location", "value",
  "roomAction", "actorAction", "contents", "tpl",
  "prepDefault", "verActor", "validDo", "validIo",
  "lookAround", "roomCheck", "statusLine", "locationOK",
  "isVisible", "cantReach", "isHim", "isHer", "action",
  "validDoList", "validIoList", "iobjGen", "dobjGen",
  "nilPrep", "rejectMultiDobj", "moveInto", "construct",
  "destruct", "validActor", "preferredActor", "isEquivalent",
  "adesc", "multisdesc", "tpl2", "anyvalue",
  "newNumbered", "unknown", "parseUnknownDobj",
  "parseUnknownIobj", "dobjCheck", "iobjCheck", "verbAction",
  "disambigDobj", "disambigIobj", "prefixdesc", "isThem"]


{-----------------------------------------------------------}


ppSpecWord specWords =
  let lines = map (join " = " . map (ppString' '\'')) specWords
      terminators = replicate (length lines - 1) "," ++ [";"]
  in "specialWords" : indentBlock (zipWith (++) lines terminators)


ppCmpd compoundWords =
  helper (map (\x -> ' ' : ppString' '\'' x) compoundWords) where
    helper (a:b:c:xs) =
      ("compoundWord" ++ a ++ b ++ c ++ ";") : helper xs
    helper [] = []


ppFmtStr formatStrings =
  map helper formatStrings where
    helper (str,prop) = "formatstring " ++ ppString' '\'' str ++ " " ++ nameProp prop ++ ";"


ppObj vocab (n,TadsObject isClass supers props) =
  firstLine : indentBlock (join [""] (filter (not.null) body)) ++ [";"]
  where
    firstLine = classPrefix ++ nameObject n ++ ": " ++ join ", " supers'
    classPrefix = if isClass then "class " else ""
    supers' = if null supers then ["object"] else map nameObject supers
    body = map ppVocab thisVocab ++ map ppProp props
    thisVocab = fromMaybe [] (lookup n vocab)

ppObj vocab (n,TadsFunction body) =
  ppFunction (nameObject n) ": function" " {" body


ppFunction name separator opener body =
  case ppCode body of
    []     -> [header ++ " }"]
    [line] -> [header ++ ' ' : line ++ " }"]
    lines  -> header : indentBlock lines ++ ["}"]
  where header = name ++ separator ++ ppParamList nameLocal body ++ opener


ppVocab (n,words) =
  [nameProp n ++ " = " ++ join " " (map (ppString' '\'') words)]


ppProp (n,TadsMethod code) =
  ppFunction (nameProp n) "" " = {" code

ppProp (n,Demand) = []		-- FIXME

ppProp (n,Synonym k) =
  case (nameProp k,nameProp n) of
    ('d':'o':k','d':'o':n') -> ["doSynonym('" ++ k' ++ "') = '" ++ n' ++ "'"]
    ('i':'o':k','i':'o':n') -> ["ioSynonym('" ++ k' ++ "') = '" ++ n' ++ "'"]
    ('v':'e':'r':_,'v':'e':'r':_) -> []	-- presumably there's a corresponding do/io entry
    (k',n') -> ["{{{ unexpected synonym " ++ k' ++ " -> " ++ n' ++ " }}}"]

ppProp (n,Redirect k) =
  case (nameProp n,nameProp k) of
    ('v':'e':'r':_,'v':'e':'r':_) -> []
    (n',k')                       -> [n' ++ " -> " ++ k']

ppProp (n,Val v) =
  [nameProp n ++ " = " ++ ppValue v]

ppProp (n,TPL2 vals) = map ppTPL2 vals


ppTPL2 (prep,(verIoVerb,_,_,doVerb),_) =
  ida ++ " = '" ++ dropDoIo (nameProp doVerb) ++ "'"
  where ida
         | verIoVerb /= 0  = "ioAction(" ++ nameObject prep ++ ")"
         | otherwise       = "doAction"


ppParamList nameLocal (CHKARGC 0 : _) = ""

ppParamList nameLocal (CHKARGC argc : _) =
  '(' : join ", " (ppParamList' nameLocal argc) ++ ")"

ppParamList _ _ = "({{{unknown param count}}})"

ppParamList' nameLocal argc
  | argc >= 128 = ppParamList' nameLocal (argc-128) ++ ["..."]
  | otherwise   = map (nameLocal.negate) [1..argc]


dropDoIo ('d':'o':rest) = rest
dropDoIo ('i':'o':rest) = rest
dropDoIo name = "{{{expected do or io: " ++ name ++ "}}}"


ppAll chunks =
  let vocab = getVocab chunks in
  let ?info = getSymbolInfo chunks vocab in
  ppAll' chunks vocab

getSymbolInfo chunks vocab =
  case concat [x | ChunkSYMTAB x <- chunks] of
    [] -> let objNames  = (65535,"nullobj") : getObjectDescs vocab chunks
              propNames = getPropDescs chunks objNames
          in makeInfo objNames propNames
    symtab -> let objNames  = (65535,"nullobj") : [(id,name) | (t,id,name) <- symtab, t `elem` [1,2]]
                  propNames = [(id,name) | (3,id,name) <- symtab]
              in makeInfo objNames propNames

makeInfo objNames propNames =
  Info (\n -> fromMaybe ("obj" ++ show n) (lookup n objNames))
       (\n -> fromMaybe ("prop" ++ show n) (lookup n propNames))


ppAll' chunks vocab =
  "#pragma C+" : concatMap (("":) . ppChunk) chunks where
    ppChunk (ChunkCMPD c)     = ppCmpd c
    ppChunk (ChunkFMTSTR f)   = ppFmtStr f
    ppChunk (ChunkOBJ objs)   = join [""] (map (ppObj vocab) objs)
    ppChunk (ChunkREQ _)      = []
    ppChunk (ChunkSPECWORD s) = ppSpecWord s
    ppChunk (ChunkVOC _)      = []
    ppChunk (ChunkSYMTAB _)   = []
    ppChunk (ChunkOther "XSI")= []
    ppChunk (ChunkOther name) = ["// unknown chunk type " ++ name]


data Info = Info { nameObject' :: Int -> String, nameProp' :: Int -> String }


{-----------------------------------------------------------}


onFst f (x,y) = (f x,y)
onSnd f (x,y) = (x,f y)


stableSortBy :: (a -> a -> Bool) -> [a] -> [a]

stableSortBy lt [] = []
stableSortBy lt lst = balancedFold (mergeBy lt) (map (\x -> [x]) lst)

balancedFold f x =
  findSingleton (iterate balancedFold' x) where
    findSingleton ([x]:_) = x
    findSingleton (x:xs) = findSingleton xs
    balancedFold' (x:y:zs) = f x y : balancedFold' zs
    balancedFold' x = x

mergeBy lt x [] = x
mergeBy lt [] y = y
mergeBy lt (x:xs) (y:ys)
  | y `lt` x   = y : mergeBy lt (x:xs) ys
  | otherwise  = x : mergeBy lt xs (y:ys)


