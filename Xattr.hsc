{-# LANGUAGE ForeignFunctionInterface #-}

module Xattr
    ( setxattr
    , lsetxattr
    , fsetxattr
    , getxattr
    , lgetxattr
    , fgetxattr
    , listxattr
    , llistxattr
    , flistxattr
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

setxattr :: String -> String -> BS.ByteString -> XattrMode -> IO ()
setxattr path = mkSetxattr "setxattr" path newCString c_setxattr

lsetxattr :: String -> String -> BS.ByteString -> XattrMode -> IO ()
lsetxattr path = mkSetxattr "lsetxattr" path newCString c_lsetxattr

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

getxattr :: String -> String -> IO BS.ByteString
getxattr path = mkGetxattr "getxattr" path newCString c_getxattr

lgetxattr :: String -> String -> IO BS.ByteString
lgetxattr path = mkGetxattr "lgetxattr" path newCString c_lgetxattr

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

listxattr :: String -> IO [String]
listxattr path = mkListxattr "listxattr" path newCString c_listxattr

llistxattr :: String -> IO [String]
llistxattr path = mkListxattr "llistxattr" path newCString c_llistxattr

flistxattr :: Handle -> IO [String]
flistxattr handle = mkListxattr "flistxattr" handle handleToIOCInt c_flistxattr
