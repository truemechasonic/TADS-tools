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


module Scads3_read (
	readRawFile,
	getPos, isEOS, getUByte, getBytes, limitBytes,
	getSByte, getUWord, getSWord, getRelWord, getDword,
	getStringN,
	getString1, getString2,
	repeatUntilEmpty
) where


import Scads3_types

import System.Exit (exitFailure)
import Data.Char (chr)
import Control.Monad (replicateM,when)
import Control.Monad.State (get,put,evalState)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr (Ptr,nullPtr,plusPtr)
import Foreign.Storable (sizeOf,peek,peekElemOff)
import Foreign.C.Types (CInt)
import Foreign.C.String (CString,withCString)


readRawFile :: FilePath -> IO DataBlock

readRawFile name =
  do buffer <- withCString name (load_and_decrypt_tads3)
     when (buffer == nullPtr) exitFailure
     length <- peek (buffer :: Ptr CInt)
     let sizeofCInt = sizeOf (undefined :: CInt)
     return (plusPtr buffer sizeofCInt, 0, fromIntegral length)

foreign import ccall "Scads3_load.h load_and_decrypt_tads3"
  load_and_decrypt_tads3 :: CString -> IO (Ptr a)


{-------------------}

getPos :: StreamReader Int
isEOS :: StreamReader Bool
getUByte :: StreamReader Int
getBytes :: Int -> StreamReader DataBlock
limitBytes :: Int -> StreamReader ()
putBounds :: DataBlock -> StreamReader ()

getPos =
  do (p,a,z) <- get
     return a

isEOS =
  do (p,a,z) <- get
     return (a >= z)

getUByte =
  do (p,a,z) <- get
     putBounds (p,a+1,z)
     return $ fromIntegral $ unsafePerformIO $ peekElemOff p a

getBytes n =
  do (p,a,z) <- get
     putBounds (p,a+n,z)
     return (p,a,a+n)

limitBytes n =
  do x <- getBytes n
     putBounds x


putBounds bounds@(p,a,z) =
  if a > z then
    error "Byte read out of bounds (bug or bad game file)"
   else
    put bounds

{-------------------}

getSByte,getUWord,getSWord,getRelWord,getDword :: StreamReader Int
getStringN :: Int -> StreamReader String
getString1,getString2 :: StreamReader String

getSByte =
  do a <- getUByte
     return (if a<128 then a else a-256)

getUWord =
  do a <- getUByte
     b <- getUByte
     return (a + b*256)

getSWord =
  do a <- getUByte
     b <- getSByte
     return (a + b*256)

getRelWord =
  do pos <- getPos
     ofs <- getSWord
     return (pos+ofs)

getDword =
  do a <- getUWord
     b <- getSWord
     return (a + b*65536)

getStringN n =
  replicateM n getUByte >>= return . map chr

getString1 = getUByte >>= getStringN
getString2 = getUWord >>= getStringN

{-------------------}

repeatUntilEmpty :: StreamReader a -> StreamReader [a]

repeatUntilEmpty action =
  do eos <- isEOS
     if eos then
       return []
      else do
       first <- action
       s <- get
       let lazyRest = evalState (repeatUntilEmpty action) s
       return (first : lazyRest)
