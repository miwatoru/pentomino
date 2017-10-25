{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_pentomino (
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

bindir     = "/Users/toru/Dropbox/haskell/pentomino/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/bin"
libdir     = "/Users/toru/Dropbox/haskell/pentomino/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/lib/x86_64-osx-ghc-8.0.2/pentomino-0.1.0.0-a9DLXPciexF1Qvu0n7WfA"
dynlibdir  = "/Users/toru/Dropbox/haskell/pentomino/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/toru/Dropbox/haskell/pentomino/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/share/x86_64-osx-ghc-8.0.2/pentomino-0.1.0.0"
libexecdir = "/Users/toru/Dropbox/haskell/pentomino/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/libexec"
sysconfdir = "/Users/toru/Dropbox/haskell/pentomino/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pentomino_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pentomino_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "pentomino_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "pentomino_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pentomino_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pentomino_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
