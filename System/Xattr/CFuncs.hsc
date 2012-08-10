
module System.Xattr.CFuncs where

import Foreign.C
import Foreign.Ptr
import Data.Bits ((.|.))
import System.Posix.Types

type Void = CChar

#ifdef __APPLE__

#include <sys/xattr.h>

foreign import ccall unsafe "setxattr" c_osx_setxattr :: CString -> CString -> Ptr Void -> CSize -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "fsetxattr" c_osx_fsetxattr :: CInt -> CString -> Ptr Void -> CSize -> CInt -> CInt -> IO CInt

c_setxattr :: CString -> CString -> Ptr Void -> CSize -> CInt -> IO CInt
c_setxattr path name value size flags = c_osx_setxattr path name value size 0 flags

c_lsetxattr :: CString -> CString -> Ptr Void -> CSize -> CInt -> IO CInt
c_lsetxattr path name value size flags = c_osx_setxattr path name value size 0 (#{const XATTR_NOFOLLOW} .|. flags)

c_fsetxattr :: CInt -> CString -> Ptr Void -> CSize -> CInt -> IO CInt
c_fsetxattr fd name value size flags = c_osx_fsetxattr fd name value size 0 flags


foreign import ccall unsafe "getxattr" c_osx_getxattr :: CString -> CString -> Ptr Void -> CSize -> CInt -> CInt -> IO CSize
foreign import ccall unsafe "fgetxattr" c_osx_fgetxattr :: CInt -> CString -> Ptr Void -> CSize -> CInt -> CInt -> IO CSize

c_getxattr :: CString -> CString -> Ptr Void -> CSize -> IO CSize
c_getxattr path name value size = c_osx_getxattr path name value size 0 0

c_lgetxattr :: CString -> CString -> Ptr Void -> CSize -> IO CSize
c_lgetxattr path name value size = c_osx_getxattr path name value size 0 #{const XATTR_NOFOLLOW}

c_fgetxattr :: CInt -> CString -> Ptr Void -> CSize -> IO CSize
c_fgetxattr fd name value size = c_osx_fgetxattr fd name value size 0 0


foreign import ccall unsafe "listxattr" c_osx_listxattr :: CString -> CString -> CSize -> CInt -> IO CSsize
foreign import ccall unsafe "flistxattr" c_osx_flistxattr :: CInt -> CString -> CSize -> CInt -> IO CSsize

c_listxattr :: CString -> CString -> CSize -> IO CSsize
c_listxattr path list size = c_osx_listxattr path list size 0

c_llistxattr :: CString -> CString -> CSize -> IO CSsize
c_llistxattr path list size = c_osx_listxattr path list size #{const XATTR_NOFOLLOW}

c_flistxattr :: CInt -> CString -> CSize -> IO CSsize
c_flistxattr fd list size = c_osx_flistxattr fd list size 0

#else

-- We assume Linux.

foreign import ccall unsafe "setxattr" c_setxattr :: CString -> CString -> Ptr Void -> CSize -> CInt -> IO CInt
foreign import ccall unsafe "lsetxattr" c_lsetxattr :: CString -> CString -> Ptr Void -> CSize -> CInt -> IO CInt
foreign import ccall unsafe "fsetxattr" c_fsetxattr :: CInt -> CString -> Ptr Void -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "getxattr" c_getxattr :: CString -> CString -> Ptr Void -> CSize -> IO CSize
foreign import ccall unsafe "lgetxattr" c_lgetxattr :: CString -> CString -> Ptr Void -> CSize -> IO CSize
foreign import ccall unsafe "fgetxattr" c_fgetxattr :: CInt -> CString -> Ptr Void -> CSize -> IO CSize

foreign import ccall unsafe "listxattr" c_listxattr :: CString -> CString -> CSize -> IO CSsize
foreign import ccall unsafe "llistxattr" c_llistxattr :: CString -> CString -> CSize -> IO CSsize
foreign import ccall unsafe "flistxattr" c_flistxattr :: CInt -> CString -> CSize -> IO CSsize

#endif
