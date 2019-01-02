{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_file_server (
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

bindir     = "/Users/colbyn/Developer/Public/file-server/.stack-work/install/x86_64-osx/lts-12.7/8.4.3/bin"
libdir     = "/Users/colbyn/Developer/Public/file-server/.stack-work/install/x86_64-osx/lts-12.7/8.4.3/lib/x86_64-osx-ghc-8.4.3/file-server-0.1.0.0-8OWCfd0BW201IYIagzXwq5"
dynlibdir  = "/Users/colbyn/Developer/Public/file-server/.stack-work/install/x86_64-osx/lts-12.7/8.4.3/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/colbyn/Developer/Public/file-server/.stack-work/install/x86_64-osx/lts-12.7/8.4.3/share/x86_64-osx-ghc-8.4.3/file-server-0.1.0.0"
libexecdir = "/Users/colbyn/Developer/Public/file-server/.stack-work/install/x86_64-osx/lts-12.7/8.4.3/libexec/x86_64-osx-ghc-8.4.3/file-server-0.1.0.0"
sysconfdir = "/Users/colbyn/Developer/Public/file-server/.stack-work/install/x86_64-osx/lts-12.7/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "file_server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "file_server_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "file_server_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "file_server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "file_server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "file_server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
