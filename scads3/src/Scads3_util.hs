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


module Scads3_util (
	fst3, onFst, onSnd,
	uniq, groupFst,
	isPrefixOf,
	setMerge, setDiff
) where


import List (partition)


fst3 :: (a,b,c) -> a
fst3 (x,y,z) = x

onFst :: (a -> b) -> (a,x) -> (b,x)
onFst f (x,y) = (f x,y)

onSnd :: (a -> b) -> (x,a) -> (x,b)
onSnd f (x,y) = (x,f y)

uniq :: (Eq a) => [a] -> [a]
uniq (x:xs) = x : uniq (dropWhile (== x) xs)
uniq []     = []

isPrefixOf :: String -> String -> Bool
(p:ps) `isPrefixOf` (s:ss) | p == s  = ps `isPrefixOf` ss
[] `isPrefixOf` s = True
_  `isPrefixOf` _ = False

groupFst :: (Eq a) => [(a,b)] -> [(a,[b])]
groupFst [] = []
groupFst ((key, val) : rest) =
  let (y,n) = partition ((key ==) . fst) rest
  in (key, val : map snd y) : groupFst n

setMerge :: (Ord a) => [a] -> [a] -> [a]
setMerge a [] = a
setMerge [] b = b
setMerge (a:as) (b:bs) =
  case a `compare` b of
    EQ -> a : setMerge as bs
    LT -> a : setMerge as (b:bs)
    GT -> b : setMerge (a:as) bs

setDiff :: (Ord a) => [a] -> [a] -> [a]
setDiff a [] = a
setDiff [] b = []
setDiff (a:as) (b:bs) =
  case a `compare` b of
    EQ -> setDiff as bs
    LT -> a : setDiff as (b:bs)
    GT -> setDiff (a:as) bs
