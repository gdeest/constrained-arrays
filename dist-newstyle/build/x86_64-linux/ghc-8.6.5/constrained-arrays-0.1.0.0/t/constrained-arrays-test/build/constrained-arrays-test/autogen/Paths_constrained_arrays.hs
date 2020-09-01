{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_constrained_arrays (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/huginn/.cabal/bin"
libdir     = "/home/huginn/.cabal/lib/x86_64-linux-ghc-8.6.5/constrained-arrays-0.1.0.0-inplace-constrained-arrays-test"
dynlibdir  = "/home/huginn/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/huginn/.cabal/share/x86_64-linux-ghc-8.6.5/constrained-arrays-0.1.0.0"
libexecdir = "/home/huginn/.cabal/libexec/x86_64-linux-ghc-8.6.5/constrained-arrays-0.1.0.0"
sysconfdir = "/home/huginn/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "constrained_arrays_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "constrained_arrays_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "constrained_arrays_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "constrained_arrays_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "constrained_arrays_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "constrained_arrays_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
