module Main (main) where

import System.Directory (getAppUserDataDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Info (arch, os)
import System.Posix.Process (executeFile)
import Text.Printf (printf)


main :: IO ()
main = do
  as <- getArgs
  p  <- program
  case as of
    ["--recompile"] -> do
      s <- source
      executeFile "ghc" True [s, "-o", p, "-O", "-threaded"] Nothing
    _ ->
      executeFile p False as Nothing



-- | ~/.pakej/pakej-x86_64-linux
program :: IO FilePath
program = appDirectory "pakej" (printf "pakej-%s-%s" arch os)

-- | ~/.pakej/pakej.hs
source :: IO FilePath
source = appDirectory "pakej" "pakej.hs"

appDirectory :: String -> FilePath -> IO FilePath
appDirectory app filename = do
  dir <- getAppUserDataDirectory app
  return (dir </> filename)
