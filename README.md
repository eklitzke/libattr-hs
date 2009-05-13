This implements bindings to SGI's libattr, which provides access to extended
attributes on Unix systems for filesystems that support them. In particular,
this provides access to extended attributes on ext3/4 and xfs on Linux (as well
as other filesystems that support this interface). For more information about
extended attributes, see the man page for attr(5).

To build this code, you'll need to have the libattr development headers
installed. On Fedora/RHEL the package name is `libattr-devel`. On Debian/Ubuntu
the package name is `attr-dev`.

You can find the latest stable version of this code on
[HackageDB](http://hackage.haskell.org/packages/hackage.html). The bleeding-edge
version of the code is hosted at git://github.com/eklitzke/libattr-hs.git.
