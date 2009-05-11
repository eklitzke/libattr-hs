{-# INCLUDE <sys/types.h> #-}
{-# INCLUDE <attr/xattr.h> #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Xattr where
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import qualified Data.ByteString as BS

type Void = CChar

allocBufSize :: Int
allocBufSize = 1024

foreign import ccall unsafe "setxattr" c_setxattr :: CString -> CString -> Ptr Void -> CSize -> CInt -> IO CInt

setxattr :: String -> String -> BS.ByteString -> Int -> IO Int
setxattr path name attrData flags = do
  cPath <- newCString path
  cName <- newCString name
  val <- BS.useAsCStringLen attrData $ \(binaryData, dataLen) -> do
      c_setxattr cPath cName binaryData (fromIntegral dataLen) (fromIntegral flags)
  return $ fromIntegral val

foreign import ccall unsafe "getxattr" c_getxattr :: CString -> CString -> Ptr Void -> CSize -> IO CSize

getxattr :: String -> String -> IO BS.ByteString
getxattr path name = do
  cPath <- newCString path
  cName <- newCString name
  bytes <- mallocBytes allocBufSize
  buflen <- c_getxattr cPath cName bytes (fromIntegral allocBufSize)
  retval <- BS.packCStringLen (bytes, fromIntegral buflen)
  free bytes
  return retval
