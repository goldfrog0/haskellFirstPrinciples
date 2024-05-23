{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_testing (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ziz/Documents/hask_proj/githubSave/chpt14/testing/.stack-work/install/x86_64-linux-tinfo6/2237a8f59cc1be96c47963e2378995aa2f1fdf5845680f583f342902abff05ba/9.6.5/bin"
libdir     = "/home/ziz/Documents/hask_proj/githubSave/chpt14/testing/.stack-work/install/x86_64-linux-tinfo6/2237a8f59cc1be96c47963e2378995aa2f1fdf5845680f583f342902abff05ba/9.6.5/lib/x86_64-linux-ghc-9.6.5/testing-0.1.0.0-8MV8uzs8yttJKn1KDodeMD-testing"
dynlibdir  = "/home/ziz/Documents/hask_proj/githubSave/chpt14/testing/.stack-work/install/x86_64-linux-tinfo6/2237a8f59cc1be96c47963e2378995aa2f1fdf5845680f583f342902abff05ba/9.6.5/lib/x86_64-linux-ghc-9.6.5"
datadir    = "/home/ziz/Documents/hask_proj/githubSave/chpt14/testing/.stack-work/install/x86_64-linux-tinfo6/2237a8f59cc1be96c47963e2378995aa2f1fdf5845680f583f342902abff05ba/9.6.5/share/x86_64-linux-ghc-9.6.5/testing-0.1.0.0"
libexecdir = "/home/ziz/Documents/hask_proj/githubSave/chpt14/testing/.stack-work/install/x86_64-linux-tinfo6/2237a8f59cc1be96c47963e2378995aa2f1fdf5845680f583f342902abff05ba/9.6.5/libexec/x86_64-linux-ghc-9.6.5/testing-0.1.0.0"
sysconfdir = "/home/ziz/Documents/hask_proj/githubSave/chpt14/testing/.stack-work/install/x86_64-linux-tinfo6/2237a8f59cc1be96c47963e2378995aa2f1fdf5845680f583f342902abff05ba/9.6.5/etc"

getBinDir     = catchIO (getEnv "testing_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "testing_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "testing_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "testing_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "testing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "testing_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
