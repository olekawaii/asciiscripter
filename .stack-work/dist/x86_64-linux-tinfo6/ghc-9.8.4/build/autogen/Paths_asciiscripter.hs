{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_asciiscripter (
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
bindir     = "/home/olekawaii/code/hs/asciiscripter/.stack-work/install/x86_64-linux-tinfo6/a3fb93355c37d17f8dc2bc34087978a3ebad0cbb040018f8a49ecbb607d43d17/9.8.4/bin"
libdir     = "/home/olekawaii/code/hs/asciiscripter/.stack-work/install/x86_64-linux-tinfo6/a3fb93355c37d17f8dc2bc34087978a3ebad0cbb040018f8a49ecbb607d43d17/9.8.4/lib/x86_64-linux-ghc-9.8.4/asciiscripter-0.1.0.0-H7qyQNL8aq31BH4kRkQ88D"
dynlibdir  = "/home/olekawaii/code/hs/asciiscripter/.stack-work/install/x86_64-linux-tinfo6/a3fb93355c37d17f8dc2bc34087978a3ebad0cbb040018f8a49ecbb607d43d17/9.8.4/lib/x86_64-linux-ghc-9.8.4"
datadir    = "/home/olekawaii/code/hs/asciiscripter/.stack-work/install/x86_64-linux-tinfo6/a3fb93355c37d17f8dc2bc34087978a3ebad0cbb040018f8a49ecbb607d43d17/9.8.4/share/x86_64-linux-ghc-9.8.4/asciiscripter-0.1.0.0"
libexecdir = "/home/olekawaii/code/hs/asciiscripter/.stack-work/install/x86_64-linux-tinfo6/a3fb93355c37d17f8dc2bc34087978a3ebad0cbb040018f8a49ecbb607d43d17/9.8.4/libexec/x86_64-linux-ghc-9.8.4/asciiscripter-0.1.0.0"
sysconfdir = "/home/olekawaii/code/hs/asciiscripter/.stack-work/install/x86_64-linux-tinfo6/a3fb93355c37d17f8dc2bc34087978a3ebad0cbb040018f8a49ecbb607d43d17/9.8.4/etc"

getBinDir     = catchIO (getEnv "asciiscripter_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "asciiscripter_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "asciiscripter_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "asciiscripter_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "asciiscripter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "asciiscripter_sysconfdir") (\_ -> return sysconfdir)



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
