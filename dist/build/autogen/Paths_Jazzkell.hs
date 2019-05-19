{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Jazzkell (
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
version = Version [0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3\\Jazzkell-0.0.1-IX33B3ntAmMJWlggRnreQ1"
dynlibdir  = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3"
datadir    = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3\\Jazzkell-0.0.1"
libexecdir = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\Jazzkell-0.0.1-IX33B3ntAmMJWlggRnreQ1\\x86_64-windows-ghc-8.4.3\\Jazzkell-0.0.1"
sysconfdir = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Jazzkell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Jazzkell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Jazzkell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Jazzkell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Jazzkell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Jazzkell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
