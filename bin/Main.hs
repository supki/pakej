{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main (main) where

import Data.Version (showVersion)
import System.Directory (getAppUserDataDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Info (arch, os)
import System.Posix.Process (executeFile)
import Text.Printf (printf)

import Paths_pakej (version)


main :: IO a
main = do
  args <- getArgs
  name <- program
  case args of
    "--recompile" : args -> recompilePakej name args
    _                    -> runPakej name args

-- | Run pakej executable with the specified arguments
runPakej :: FilePath -> [String] -> IO a
runPakej path args = executeFile path False args Nothing

-- | Recompile pakej sources and place the result somewhere
recompilePakej :: FilePath -> [String] -> IO a
recompilePakej dst args = do
  s <- source
  executeFile "ghc" True ([s, "-o", dst, "-O", "-threaded"] ++ args) Nothing

-- | ~/.pakej/pakej-x86_64-linux
program :: IO FilePath
program = appDirectory "pakej" (printf "pakej-%s-%s-%s" arch os (showVersion version))

-- | ~/.pakej/pakej.hs
source :: IO FilePath
source = appDirectory "pakej" "pakej.hs"

appDirectory :: String -> FilePath -> IO FilePath
appDirectory app filename = do
  dir <- getAppUserDataDirectory app
  return (dir </> filename)
