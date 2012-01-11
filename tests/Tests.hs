{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

import Data.List (sort)
import Control.Monad (forM_)
import Control.Exception (bracket)
import System.IO
import System.Directory
import System.Posix.Temp
import System.Xattr
import qualified Data.ByteString.Char8 as BC
import System.IO.Unsafe

main :: IO ()
main = defaultMain tests

name :: AttrName
name = "user.xattrName"

value :: AttrValue
value = "xattrValue\255\0\1\2Ends\42\43Here"

nrAttrs :: Int
nrAttrs = 10

tests :: [Test]
tests = [ 
    testCase "set_get_xattr"    test_set_get_xattr
  , testCase "fset_fget_xattr"  test_fset_fget_xattr
  , testCase "set_fget_xattr"   test_set_fget_xattr
  , testCase "fset_get_xattr"   test_fset_get_xattr
  , testCase "set_list_xattr"   test_set_list_xattr
  , testCase "fset_flist_xattr" test_fset_flist_xattr
  ]

test_set_get_xattr :: Assertion
{-# NOINLINE test_set_get_xattr #-}
test_set_get_xattr = unsafePerformIO $ do
  withTmpFile $ \(path,_) -> do
    setxattr path name value RegularMode
    value' <- getxattr path name
    return $ value' @?= value

test_fset_fget_xattr :: Assertion
{-# NOINLINE test_fset_fget_xattr #-}
test_fset_fget_xattr = unsafePerformIO $ do
  withTmpFile $ \(_,h) -> do
    fsetxattr h name value RegularMode
    value' <- fgetxattr h name
    return $ value' @?= value

test_set_fget_xattr :: Assertion
{-# NOINLINE test_set_fget_xattr #-}
test_set_fget_xattr = unsafePerformIO $ do
  withTmpFile $ \(path,h) -> do
    setxattr path name value RegularMode
    value' <- fgetxattr h name
    return $ value' @?= value

test_fset_get_xattr :: Assertion
{-# NOINLINE test_fset_get_xattr #-}
test_fset_get_xattr = unsafePerformIO $ do
  withTmpFile $ \(path,h) -> do
    fsetxattr h name value RegularMode
    value' <- getxattr path name
    return $ value' @?= value

test_set_list_xattr :: Assertion
{-# NOINLINE test_set_list_xattr #-}
test_set_list_xattr = unsafePerformIO $ do
  withTmpFile $ \(path,_) -> do
    let attrs = map (\x -> name ++ show x) [1..nrAttrs]
    forM_ attrs $ \namei -> 
        setxattr path namei value RegularMode
    attrs' <- listxattr path
    return $ sort attrs' @?= sort attrs
  
test_fset_flist_xattr :: Assertion
{-# NOINLINE test_fset_flist_xattr #-}
test_fset_flist_xattr = unsafePerformIO $ do
  withTmpFile $ \(_,h) -> do
    let attrs = map (\x -> name ++ show x) [1..nrAttrs]
    forM_ attrs $ \namei -> 
        fsetxattr h namei value RegularMode
    attrs' <- flistxattr h
    return $ sort attrs' @?= sort attrs

withTmpFile :: ((FilePath, Handle) -> IO Assertion) -> IO Assertion
withTmpFile io = do
  bracket (mkstemp "xattrXXXXXX")  -- works with old version of mkstemp
          (\(path,h) -> hClose h >> removeFile path)
          (\(path,h) -> BC.hPutStr h "testing xattr" >> io (path,h))
