module System.Xattr.Types
    (
    -- * Data Types
      AttrName
    , XattrMode(RegularMode,CreateMode,ReplaceMode)
    )
    where

#include <sys/types.h>

#if defined (__APPLE__) || defined (linux)
#include <sys/xattr.h>
#else
#include <attr/xattr.h>
#endif

-- |Represents the mode for an update (i.e. set) operation
data XattrMode
    = RegularMode -- ^ The attribute will be created if it does not yet exist, and replace the existing named attribute otherwise.
    | CreateMode  -- ^ Specifies a pure create, which fails if the named attribute exists already.
    | ReplaceMode -- ^ Specifies a pure replace operation, which fails if the named attribute does not already exist.
    deriving (Eq, Show)

instance Enum XattrMode where
    fromEnum RegularMode = 0
    fromEnum CreateMode  = #{const XATTR_CREATE}
    fromEnum ReplaceMode = #{const XATTR_REPLACE}
    toEnum 0                      = RegularMode
    toEnum #{const XATTR_CREATE}  = CreateMode
    toEnum #{const XATTR_REPLACE} = ReplaceMode
    toEnum _                      = error "Unknown XattrMode"

-- |The name of an attribute. Some filesystems support arbitrarily long names,
-- but for portability you're recommended to limit this to 255 bytes.
type AttrName = String 

