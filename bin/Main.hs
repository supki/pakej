{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Control.Applicative
import           Control.Exception (IOException)
import           Control.Monad
import           Data.Foldable (for_, traverse_)
import           Data.List (isSuffixOf)
import           Data.Maybe (catMaybes)
import           Data.Traversable (traverse)
import           Data.Version (showVersion)
import           System.Directory (copyFile, createDirectoryIfMissing, getAppUserDataDirectory, getDirectoryContents)
import           System.Environment (getArgs)
import           System.Exit (exitWith, exitFailure)
import           System.FilePath ((</>), takeDirectory)
import           System.Info (arch, os)
import           System.IO (hPutStrLn, stderr)
import           System.IO.Error (catchIOError)
import qualified System.Posix as Posix
import           System.Process (runProcess, waitForProcess)
import           Text.Printf (printf)

import           Paths_pakej (version, getDataFileName)


main :: IO ()
main = getArgs >>= \case
  ["--init"]           -> initPakej
  "--recompile" : args -> program >>= recompilePakej args
  args                 -> program >>= runPakej args

-- | Create Pakej app directory and copy pakej.hs template over
initPakej :: IO ()
initPakej = do
  s <- sourceFile
  createDirectoryIfMissing True (takeDirectory s)
  t <- getDataFileName "data/pakej.hs"
  copyFile t s

-- | Recompile pakej sources and place the result somewhere
recompilePakej :: [String] -> FilePath -> IO a
recompilePakej args dst = do
  source <- sourceFile
  appDir <- pakejDirectory
  exitWith =<<
    waitForProcess =<<
      runProcess "cabal" (cabalOpts source ++ args) (Just appDir) Nothing Nothing Nothing Nothing
 where
  cabalOpts s = ["exec", "ghc", "--", "-odir", "obj", "-hidir", "obj", s, "-o", dst, "-O", "-threaded"]

-- | Run pakej executable with the specified arguments
runPakej :: [String] -> FilePath -> IO a
runPakej args path =
  getModificationTime path >>= \case
    Nothing ->
      die "Pakej executable is missing! Did you forget to run\n\
          \    pakej --recompile\n\
          \?"
    Just mtime ->
      pakejDirectory >>= getSources >>= fmap catMaybes . traverse getModificationTime >>= \mtimes -> do
        for_ (traverse_ (guard . (> mtime)) mtimes :: Maybe ()) $ \_ ->
          warn "Some source files are newer than the pakej executable\n\
                \To rebuild it, please run\n\
                \    pakej --recompile\n\
                \!"
        Posix.executeFile path False args Nothing

getSources :: FilePath -> IO [FilePath]
getSources file = do
  getUsefulDirectoryContents file >>= fmap concat . traverse getSources
 `catchIOError`
  \_ -> return [file | any (`isSuffixOf` file) [".hs", ".lhs", ".hsc"]]
{-# ANN getSources "HLint: ignore Redundant do" #-}

getUsefulDirectoryContents :: FilePath -> IO [FilePath]
getUsefulDirectoryContents dir =
  (map (\file -> dir </> file) . filter (`notElem` [".", ".."])) <$> getDirectoryContents dir

getModificationTime :: FilePath -> IO (Maybe Posix.EpochTime)
getModificationTime =
  handleIOError (\_ -> return Nothing) . fmap (Just . Posix.modificationTime) . Posix.getFileStatus

handleIOError :: (IOException -> IO a) -> IO a -> IO a
handleIOError = flip catchIOError

die :: String -> IO a
die m = warn m >> exitFailure

warn :: String -> IO ()
warn = hPutStrLn stderr

-- | ~/.pakej/pakej-x86_64-linux
program :: IO FilePath
program = inPakejDirectory (printf "pakej-%s-%s-%s" arch os (showVersion version))

-- | ~/.pakej/pakej.hs
sourceFile :: IO FilePath
sourceFile = inPakejDirectory "pakej.hs"

inPakejDirectory :: FilePath -> IO FilePath
inPakejDirectory path = fmap (</> path) pakejDirectory

pakejDirectory :: IO FilePath
pakejDirectory = getAppUserDataDirectory "pakej"
