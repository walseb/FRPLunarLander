{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Project9 (
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

bindir     = "/home/admin/.cabal/bin"
libdir     = "/home/admin/.cabal/lib/x86_64-linux-ghc-8.6.4/Project9-0.1.0.0-EwsKUMwHhrIHpFLuIpKSlG-Project9"
dynlibdir  = "/home/admin/.cabal/lib/x86_64-linux-ghc-8.6.4"
datadir    = "/home/admin/.cabal/share/x86_64-linux-ghc-8.6.4/Project9-0.1.0.0"
libexecdir = "/home/admin/.cabal/libexec/x86_64-linux-ghc-8.6.4/Project9-0.1.0.0"
sysconfdir = "/home/admin/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Project9_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Project9_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Project9_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Project9_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Project9_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Project9_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
