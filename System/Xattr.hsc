#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
--------------------------------------------------------------------------------
-- |
-- Module : System.Xattr
-- Copyright : (c) Evan Klitzke 2009
--             (c) Deian Stefan 2012
-- License : BSD3
-- Maintainer: Evan Klitzke <evan@eklitzke.org>
--             Deian Stefan <deian@cs.stanford.edu>
-- Stability : experimental
-- Portability : GHC only
--
-- Relatively low-level interface to work with extended attributes on Unix
-- systems. This is a fairly straightforward port of the API exposed by SGI's
-- libattr.
--
--------------------------------------------------------------------------------
#include "HsXattrConfig.h"

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
    , AttrValue
    , XattrMode(..)
    )
    where

#include <sys/types.h>
#ifdef __APPLE__
#include <sys/xattr.h>
#else
#include <attr/xattr.h>
#endif

import Data.Functor ((<$>))
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import System.Posix.Types
import System.Posix.IO
import System.IO

import Data.ByteString (ByteString
                       , useAsCStringLen
                       , packCStringLen
                       , split)
import Data.ByteString.Char8 (unpack)
import System.Xattr.CFuncs

-- | Mode for setting attributes.
data XattrMode =
      RegularMode
    -- ^ The attribute will be created if it does not yet exist,
    -- and replace the existing named attribute otherwise.
    | CreateMode
    -- ^ Specifies a pure create, which fails if the named attribute
    -- exists already.
    | ReplaceMode
    -- ^ Specifies a pure replace operation, which fails if the
    -- named attribute does not already exist.
    deriving (Eq, Show)

instance Enum XattrMode where
    fromEnum RegularMode = 0
    fromEnum CreateMode  = #{const XATTR_CREATE}
    fromEnum ReplaceMode = #{const XATTR_REPLACE}
    toEnum #{const XATTR_CREATE}  = CreateMode
    toEnum #{const XATTR_REPLACE} = ReplaceMode
    toEnum _                      = RegularMode

-- | The name of an attribute. Some filesystems support arbitrarily
-- long names, but for portability it is recommended to use relatively
-- short names (less than 256 bytes).
type AttrName = String 

-- | Thevalue of an attribute. Most filesystems allow for arbitrary
-- binary data with relatively. It is recommended that the length of
-- the value be at most 64KB.
type AttrValue = ByteString

--
-- Set extended attributes
--

-- | High level wrapper for a @setxattr@ variant
mkSetxattr :: Show cStringOrCInt =>  String            -- ^ Function name
           -> cStringOrCInt     -- ^ Filepath ('CString') or handle ('CInt')
           -> (cStringOrCInt -> CString -> Ptr Void -> CSize -> CInt -> IO CInt)
           -> AttrName          -- ^ Attribute name
           -> AttrValue         -- ^ New value
           -> XattrMode         -- ^ Mode
           -> IO ()
mkSetxattr funcName pathOrHandle cFunc attrName attrData mode =
  throwErrnoIfMinus1_ funcName $ withCString attrName $ \cName ->
    useAsCStringLen attrData $ \(cVal, cValLen) ->
      let l = fromIntegral cValLen
          m = fromIntegral . fromEnum $ mode
      in cFunc pathOrHandle cName cVal l m

-- | Set extended attribute of a filesystem object.
setxattr :: FilePath    -- ^ Object path
         -> AttrName    -- ^ Attribute name
         -> AttrValue   -- ^ Value
         -> XattrMode   -- ^ Mode
         -> IO ()
#if HAVE_SETXATTR
setxattr path name val mode = withCString path $ \cName ->
  mkSetxattr "setxattr" cName c_setxattr name val mode
#else
setxattr = error "System.Xattr.setxattr: not supported"
#endif


-- | Same as 'setxattr', but if the object is a symbolic link the
-- attribute is set on the symbolic link itself, not the object
-- refered to by the link.
lsetxattr :: FilePath -> AttrName -> AttrValue -> XattrMode -> IO ()
#if HAVE_LSETXATTR
lsetxattr path name val mode = withCString path $ \cName ->
  mkSetxattr "lsetxattr" cName c_lsetxattr name val mode

foreign import ccall unsafe "lsetxattr"
  c_lsetxattr :: CString -> CString -> Ptr Void -> CSize -> CInt -> IO CInt
#else
lsetxattr = error "System.Xattr.lsetxattr: not supported"
#endif

-- | Same as 'setxattr', but set the attribute of an open handle.
fsetxattr :: Handle -> AttrName -> AttrValue -> XattrMode -> IO ()
#if HAVE_FSETXATTR
fsetxattr handle name val mode = handleToFd handle >>= \fd ->
  mkSetxattr "fsetxattr" (fromIntegral fd) c_fsetxattr name val mode
#else
fsetxattr = error "System.Xattr.fsetxattr: not supported"
#endif

--
-- Get extended attributes
--

-- | High level wrapper for a @getxattr@ variant
mkGetxattr :: String            -- ^ Function name
           -> cStringOrCInt     -- ^ Filepath ('CString') or handle ('CInt')
           -> (cStringOrCInt -> CString -> Ptr Void -> CSize -> IO CSize)
           -> AttrName          -- ^ Attribute name
           -> IO AttrValue
mkGetxattr funcName pathOrHandle cFunc attrName = do
  withCString attrName $ \cName -> do
    len <- throwErrnoIfMinus1 funcName $
      cFunc pathOrHandle cName (nullPtr) 0
    allocaBytes (fromIntegral len) $ \mem -> do
      len' <- throwErrnoIfMinus1 funcName $
        cFunc pathOrHandle cName mem (fromIntegral len)
      packCStringLen (mem, fromIntegral len')

-- | Get extended attribute of an object.
getxattr :: FilePath -> AttrName -> IO AttrValue
#if HAVE_GETXATTR
getxattr path name = withCString path $ \cName ->
  mkGetxattr "getxattr" cName c_getxattr name
#else
getxattr = error "System.Xattr.getxattr: not supported"
#endif

-- | Same as 'getxattr', but if the object is a symbolic link, the
-- attribute is retrieved from the link itself and not the referenced
-- object.
lgetxattr :: FilePath -> AttrName -> IO AttrValue
#if HAVE_LGETXATTR
lgetxattr path name = withCString path $ \cName ->
  mkGetxattr "lgetxattr" cName c_lgetxattr name
#else
lgetxattr = error "System.Xattr.lgetxattr: not supported"
#endif

-- | Same as 'getxattr', but get the attribute of an open handle.
fgetxattr :: Handle -> AttrName -> IO AttrValue
#if HAVE_FGETXATTR
fgetxattr handle name = handleToFd handle >>= \fd ->
  mkGetxattr "fgetxattr" (fromIntegral fd) c_fgetxattr name
#else
fgetxattr = error "System.Xattr.fgetxattr: not supported"
#endif

--
-- List extended attributes
--

-- | High level wrapper for a @listxattr@ variant
mkListxattr :: String            -- ^ Function name
            -> cStringOrCInt     -- ^ Filepath ('CString') or handle ('CInt')
            -> (cStringOrCInt -> CString -> CSize -> IO CSsize)
            -> IO [AttrName]
mkListxattr funcName pathOrHandle cFunc = do
  len <- throwErrnoIfMinus1 funcName $
    cFunc pathOrHandle (nullPtr) 0
  allocaBytes (fromIntegral len) $ \mem -> do
    len' <- throwErrnoIfMinus1 funcName $
      cFunc pathOrHandle mem (fromIntegral len)
    splitNull <$> packCStringLen (mem, fromIntegral len')
  where splitNull s = filter (/= "") $ map unpack $ split 0x0 s

-- | Get a list of all of the extended attributes of an object.
listxattr :: FilePath -> IO [AttrName]
#if HAVE_LISTXATTR
listxattr path = withCString path $ \cName ->
  mkListxattr "listxattr" cName c_listxattr
#else
listxattr = error "System.Xattr.listxattr: not supported"
#endif

-- | Same as 'listxattr', but if the object is a symbolic link, the
-- attributes of the link itself are returned, not on the referenced object.
llistxattr :: FilePath -> IO [AttrName]
#if HAVE_LLISTXATTR
llistxattr path = withCString path $ \cName ->
  mkListxattr "llistxattr" cName c_llistxattr
#else
llistxattr = error "System.Xattr.llistxattr: not supported"
#endif

-- | Same as 'listxattr', but get the attributes of an open handle.
flistxattr :: Handle -> IO [AttrName]
#if HAVE_FLISTXATTR
flistxattr handle = handleToFd handle >>= \fd ->
  mkListxattr "flistxattr" (fromIntegral fd) c_flistxattr
#else
flistxattr = error "System.Xattr.flistxattr: not supported"
#endif

