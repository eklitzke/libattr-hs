module System.Xattr.Types
    (
    -- * Data Types
      XattrMode(RegularMode,CreateMode,ReplaceMode)
    )
    where

#include <sys/types.h>
#include <attr/xattr.h>

data XattrMode = RegularMode | CreateMode | ReplaceMode

instance Enum XattrMode where
    fromEnum RegularMode = 0
    fromEnum CreateMode  = #{const XATTR_CREATE}
    fromEnum ReplaceMode = #{const XATTR_REPLACE}
    toEnum 0                      = RegularMode
    toEnum #{const XATTR_CREATE}  = CreateMode
    toEnum #{const XATTR_REPLACE} = ReplaceMode
