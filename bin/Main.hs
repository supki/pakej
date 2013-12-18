module Main (main) where

import Data.Monoid (mempty)
import Options.Applicative
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>))
import System.Info (arch, os)
import System.Posix.Process (executeFile)
import Text.Printf (printf)


main :: IO ()
main = do
  c <- conf
  p <- program
  case c of
    Recompile -> do
      s <- source
      executeFile "ghc" True [s, "-o", p, "-O", "-threaded"] Nothing
    Pass as   ->
      executeFile p False as Nothing


data Conf =
    Recompile
  | Pass [String]

conf :: IO Conf
conf = customExecParser (prefs showHelpOnError) (info (helper <*> parser) fullDesc)
 where
  parser =
        flag' Recompile (long "recompile")
    <|> Pass <$> many (argument Just mempty)


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
