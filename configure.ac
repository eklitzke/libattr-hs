#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([System.Xattr], [0.6.0], [evan@eklitzke.org, deian@cs.stanford.edu])

# Checks for programs.
AC_PROG_CC

AC_CONFIG_HEADERS([include/HsXattrConfig.h])

# Checks for header files.
AC_CHECK_HEADERS([attr/xattr.h])

# Serch for libattr library
AC_CHECK_LIB(attr, getxattr)

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_CHECK_FUNCS([getxattr lgetxattr fgetxattr])
AC_CHECK_FUNCS([setxattr lsetxattr fsetxattr])
AC_CHECK_FUNCS([listxattr llistxattr flistxattr])

AC_OUTPUT
