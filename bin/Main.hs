{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main (main) where

import Data.Version (showVersion)
import System.Directory (copyFile, createDirectoryIfMissing, getAppUserDataDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)
import System.Info (arch, os)
import System.Posix.Process (executeFile)
import Text.Printf (printf)

import Paths_pakej (version, getDataFileName)


main :: IO ()
main = do
  args <- getArgs
  name <- program
  case args of
    ["--init"]           -> initPakej
    "--recompile" : args -> recompilePakej name args
    _                    -> runPakej name args

-- | Create Pakej app directory and copy pakej.hs template over
initPakej :: IO ()
initPakej = do
  s <- source
  createDirectoryIfMissing True (takeDirectory s)
  t <- getDataFileName "data/pakej.hs"
  copyFile t s

-- | Recompile pakej sources and place the result somewhere
recompilePakej :: FilePath -> [String] -> IO a
recompilePakej dst args = do
  s <- source
  executeFile "ghc" True ([s, "-o", dst, "-O", "-threaded"] ++ args) Nothing

-- | Run pakej executable with the specified arguments
runPakej :: FilePath -> [String] -> IO a
runPakej path args = executeFile path False args Nothing

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
