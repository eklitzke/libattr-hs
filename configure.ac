#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([System.Xattr], [0.5.2], [evan@eklitzke.org])

# Checks for programs.
AC_PROG_AWK
AC_PROG_CC

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([sys/types.h attr/xattr.h])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_CHECK_FUNCS([getxattr lgetxattr fgetxattr])
AC_CHECK_FUNCS([setxattr lsetxattr fsetxattr])
AC_CHECK_FUNCS([listxattr llistxattr flistxattr])
