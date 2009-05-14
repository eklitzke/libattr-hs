--------------------------------------------------------------------------------
-- |
-- Module : Sound.Xattr
-- Copyright : (c) Evan Klitzke 2009
-- License : BSD3
-- Maintainer: Evan Klitzke <evan@eklitzke.org>
-- Stability : experimental
-- Portability : only tested with GHC
--
-- Relatively low-level interface to work with extended attributes on Unix
-- systems. This is a fairly straightforward port of the API exposed by SGI's
-- libattr.
--
--------------------------------------------------------------------------------

module System.Xattr
    (
    -- * Functions
      setxattr
    , lsetxattr
    , fsetxattr
    , getxattr
    , lgetxattr
    , fgetxattr
    , listxattr
    , llistxattr
    , flistxattr

    -- * Data Types
    , XattrMode(RegularMode,CreateMode,ReplaceMode)
    )
    where

#include <sys/types.h>
#include <attr/xattr.h>

import Data.Char
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import System.Posix.Types
import System.Posix.IO
import System.IO

import qualified Data.ByteString as BS

type Void = CChar

data XattrMode = RegularMode | CreateMode | ReplaceMode

instance Enum XattrMode where
    fromEnum RegularMode = 0
    fromEnum CreateMode  = #{const XATTR_CREATE}
    fromEnum ReplaceMode = #{const XATTR_REPLACE}
    toEnum 0                      = RegularMode
    toEnum #{const XATTR_CREATE}  = CreateMode
    toEnum #{const XATTR_REPLACE} = ReplaceMode

allocBufSize :: Int
allocBufSize = 1024

allocCSize :: CSize
allocCSize = fromIntegral allocBufSize

foreign import ccall unsafe "setxattr" c_setxattr :: CString -> CString -> Ptr Void -> CSize -> CInt -> IO CInt
foreign import ccall unsafe "lsetxattr" c_lsetxattr :: CString -> CString -> Ptr Void -> CSize -> CInt -> IO CInt
foreign import ccall unsafe "fsetxattr" c_fsetxattr :: CInt -> CString -> Ptr Void -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "getxattr" c_getxattr :: CString -> CString -> Ptr Void -> CSize -> IO CSize
foreign import ccall unsafe "lgetxattr" c_lgetxattr :: CString -> CString -> Ptr Void -> CSize -> IO CSize
foreign import ccall unsafe "fgetxattr" c_fgetxattr :: CInt -> CString -> Ptr Void -> CSize -> IO CSize

foreign import ccall unsafe "listxattr" c_listxattr :: CString -> CString -> CSize -> IO CSsize
foreign import ccall unsafe "llistxattr" c_llistxattr :: CString -> CString -> CSize -> IO CSsize
foreign import ccall unsafe "flistxattr" c_flistxattr :: CInt -> CString -> CSize -> IO CSsize

-- return a high level wrapper for a setxattr variant
mkSetxattr :: String -> a -> (a -> IO b) -> (b -> CString -> Ptr Void -> CSize -> CInt -> IO CInt) -> String -> BS.ByteString -> XattrMode -> IO ()
mkSetxattr funcName x iox cFunc attrName attrData mode = do
  x' <- iox x
  cName <- newCString attrName
  val <- BS.useAsCStringLen attrData $ \(binaryData, dataLen) ->
				    cFunc x' cName binaryData (fromIntegral dataLen) (fromIntegral $ fromEnum mode)
  if val /= 0
     then throwErrno funcName
     else return ()

handleToIOCInt :: Handle -> IO CInt
handleToIOCInt = fmap fromIntegral . handleToFd

-- |Set an attribute on a regular file, by path
setxattr :: String -> String -> BS.ByteString -> XattrMode -> IO ()
setxattr path = mkSetxattr "setxattr" path newCString c_setxattr

-- |Like setxattr, but if the path is a symbolic link set the attribute on the link itself (not the file pointed to by the link)
lsetxattr :: String -> String -> BS.ByteString -> XattrMode -> IO ()
lsetxattr path = mkSetxattr "lsetxattr" path newCString c_lsetxattr

-- |Like setxattr, but use the handle specified rather than a file path
fsetxattr :: Handle -> String -> BS.ByteString -> XattrMode -> IO ()
fsetxattr handle = mkSetxattr "fsetxattr" handle handleToIOCInt c_fsetxattr

-- return a high level wrapper for a getxattr variant
mkGetxattr :: String -> a -> (a -> IO b) -> (b -> CString -> Ptr Void -> CSize -> IO CSize) -> String -> IO BS.ByteString
mkGetxattr funcName x iox cFunc attrName = do
  x' <- iox x
  cName <- newCString attrName
  allocaBytes allocBufSize $ \mem -> do
		       buflen <- cFunc x' cName mem allocCSize
		       if buflen == -1
			  then throwErrno funcName
			  else BS.packCStringLen (mem, fromIntegral buflen)

-- |Get an attribute on a regular file, by path
getxattr :: String -> String -> IO BS.ByteString
getxattr path = mkGetxattr "getxattr" path newCString c_getxattr

-- |Like getxattr, but if the path is a symbolic link get the attribute on the link itself (not the file pointed to by the link)
lgetxattr :: String -> String -> IO BS.ByteString
lgetxattr path = mkGetxattr "lgetxattr" path newCString c_lgetxattr

-- |Like getxattr, but use the handle specified rather than a file path
fgetxattr :: Handle -> String -> IO BS.ByteString
fgetxattr handle = mkGetxattr "fgetxattr" handle handleToIOCInt c_fgetxattr

-- split a string on NUL characters
splitNull :: String -> [String]
splitNull [] = []
splitNull s  = case suf of
		 "" -> [pre]
		 _  -> pre : (splitNull $ tail suf)
    where
      (pre, suf) = break (\c -> ord c == 0) s

mkListxattr :: String -> a -> (a -> IO b) -> (b -> CString -> CSize -> IO CSsize) -> IO [String]
mkListxattr funcName x iox cFunc = do
  x' <- iox x
  allocaBytes allocBufSize $ \mem -> do
		       buflen <- cFunc x' mem allocCSize
		       if buflen == -1
			  then throwErrno funcName
			  else do s <- peekCStringLen (mem, fromIntegral buflen)
				  return $ splitNull s

-- |Get a list of all of the attributes set on a path
listxattr :: String -> IO [String]
listxattr path = mkListxattr "listxattr" path newCString c_listxattr

-- |Like listxattr, but if the path is a symbolic link get the attributes on the link itsel (not the file pointed to by the link)
llistxattr :: String -> IO [String]
llistxattr path = mkListxattr "llistxattr" path newCString c_llistxattr

-- |Like listxattr, but use the handle specified rather than a file path
flistxattr :: Handle -> IO [String]
flistxattr handle = mkListxattr "flistxattr" handle handleToIOCInt c_flistxattr
