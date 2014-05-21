{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main (main) where

import Data.Version (showVersion)
import System.Directory (copyFile, createDirectoryIfMissing, getAppUserDataDirectory)
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.FilePath ((</>), takeDirectory)
import System.Info (arch, os)
import System.Posix.Process (executeFile)
import System.Process (runProcess, waitForProcess)
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
  s <- sourceFile
  createDirectoryIfMissing True (takeDirectory s)
  t <- getDataFileName "data/pakej.hs"
  copyFile t s

-- | Recompile pakej sources and place the result somewhere
recompilePakej :: FilePath -> [String] -> IO a
recompilePakej dst args = do
  source <- sourceFile
  appDir <- appDirectory
  exitWith =<<
    waitForProcess =<<
      runProcess "cabal" (["exec", "ghc", "--", source, "-o", dst, "-O", "-threaded"] ++ args)
        (Just appDir) Nothing Nothing Nothing Nothing

-- | Run pakej executable with the specified arguments
runPakej :: FilePath -> [String] -> IO a
runPakej path args = executeFile path False args Nothing

-- | ~/.pakej/pakej-x86_64-linux
program :: IO FilePath
program = inAppDirectory (printf "pakej-%s-%s-%s" arch os (showVersion version))

-- | ~/.pakej/pakej.hs
sourceFile :: IO FilePath
sourceFile = inAppDirectory "pakej.hs"

inAppDirectory :: FilePath -> IO FilePath
inAppDirectory path = fmap (</> path) appDirectory

appDirectory :: IO FilePath
appDirectory = getAppUserDataDirectory "pakej"
