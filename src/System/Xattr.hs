--------------------------------------------------------------------------------
-- |
-- Module : System.Xattr
-- Copyright : (c) Evan Klitzke 2009
-- License : BSD3
-- Maintainer: Evan Klitzke <evan@eklitzke.org>
-- Stability : experimental
-- Portability : GHC only
--
-- Relatively low-level interface to work with extended attributes on Unix
-- systems. This is a fairly straightforward port of the API exposed by SGI's
-- libattr.
--
--------------------------------------------------------------------------------

module System.Xattr
    (
    -- * Functions
    -- ** Set Functions
      setxattr
    , lsetxattr
    , fsetxattr

    -- ** Get Functions
    , getxattr
    , lgetxattr
    , fgetxattr

    -- ** List Functions
    , listxattr
    , llistxattr
    , flistxattr

    -- * Data Types
    , AttrName
    , XattrMode(RegularMode,CreateMode,ReplaceMode)
    )
    where

import Data.Char
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import System.Posix.Types
import System.Posix.IO
import System.IO
import System.Xattr.Types

import Data.ByteString (ByteString, useAsCStringLen, packCStringLen)

type Void = CChar

allocBufSize :: Int
allocBufSize = 4096

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
mkSetxattr :: String -> a -> (a -> IO b) -> (b -> CString -> Ptr Void -> CSize -> CInt -> IO CInt) -> AttrName -> ByteString -> XattrMode -> IO ()
mkSetxattr funcName x iox cFunc attrName attrData mode = do
  x' <- iox x
  cName <- newCString attrName
  val <- useAsCStringLen attrData $ \(binaryData, dataLen) ->
                                                         cFunc x' cName binaryData (fromIntegral dataLen) (fromIntegral $ fromEnum mode)
  if val /= 0
     then throwErrno funcName
     else return ()

handleToIOCInt :: Handle -> IO CInt
handleToIOCInt = fmap fromIntegral . handleToFd

-- |Set an attribute on a regular file, by path
setxattr :: FilePath -> AttrName -> ByteString -> XattrMode -> IO ()
setxattr path = mkSetxattr "setxattr" path newCString c_setxattr

-- |Like setxattr, but if the path is a symbolic link set the attribute on the link itself (not the file pointed to by the link)
lsetxattr :: FilePath -> AttrName -> ByteString -> XattrMode -> IO ()
lsetxattr path = mkSetxattr "lsetxattr" path newCString c_lsetxattr

-- |Like setxattr, but use the handle specified rather than a file path
fsetxattr :: Handle -> AttrName -> ByteString -> XattrMode -> IO ()
fsetxattr handle = mkSetxattr "fsetxattr" handle handleToIOCInt c_fsetxattr

-- return a high level wrapper for a getxattr variant
mkGetxattr :: String -> a -> (a -> IO b) -> (b -> CString -> Ptr Void -> CSize -> IO CSize) -> AttrName -> IO ByteString
mkGetxattr funcName x iox cFunc attrName = do
  x' <- iox x
  cName <- newCString attrName
  allocaBytes allocBufSize $ \mem -> do
    buflen <- cFunc x' cName mem allocCSize
    if buflen == -1
       then throwErrno funcName
       else packCStringLen (mem, fromIntegral buflen)

-- |Get an attribute on a regular file, by path
getxattr :: FilePath -> AttrName -> IO ByteString
getxattr path = mkGetxattr "getxattr" path newCString c_getxattr

-- |Like getxattr, but if the path is a symbolic link get the attribute on the link itself (not the file pointed to by the link)
lgetxattr :: FilePath -> AttrName -> IO ByteString
lgetxattr path = mkGetxattr "lgetxattr" path newCString c_lgetxattr

-- |Like getxattr, but use the handle specified rather than a file path
fgetxattr :: Handle -> AttrName -> IO ByteString
fgetxattr handle = mkGetxattr "fgetxattr" handle handleToIOCInt c_fgetxattr

-- split a string on NUL characters
splitNull :: String -> [String]
splitNull [] = []
splitNull s  = case suf of
                 "" -> [pre]
                 _  -> pre : (splitNull $ tail suf)
    where (pre, suf) = break (\c -> ord c == 0) s

mkListxattr :: String -> a -> (a -> IO b) -> (b -> CString -> CSize -> IO CSsize) -> IO [AttrName]
mkListxattr funcName x iox cFunc = do
  x' <- iox x
  allocaBytes allocBufSize $ \mem -> do buflen <- cFunc x' mem allocCSize
                                        if buflen == -1
                                           then throwErrno funcName
                                           else fmap splitNull $ peekCStringLen (mem, fromIntegral buflen)

-- |Get a list of all of the attributes set on a path
listxattr :: FilePath -> IO [AttrName]
listxattr path = mkListxattr "listxattr" path newCString c_listxattr

-- |Like listxattr, but if the path is a symbolic link get the attributes on the link itself (not the file pointed to by the link)
llistxattr :: FilePath -> IO [AttrName]
llistxattr path = mkListxattr "llistxattr" path newCString c_llistxattr

-- |Like listxattr, but use the handle specified rather than a file path
flistxattr :: Handle -> IO [AttrName]
flistxattr handle = mkListxattr "flistxattr" handle handleToIOCInt c_flistxattr
