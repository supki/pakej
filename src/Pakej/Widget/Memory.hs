{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Pakej.Widget.Memory
  ( memoryData
#ifdef TEST
  , parseData
  , parseLine
  , memoryDataError
#endif
  ) where

import           Data.Int (Int64)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Traversable (traverse)
import           System.IO.Error (catchIOError)


memoryData :: FilePath -> IO (Either Text (HashMap Text Int64))
memoryData = handleIOError (return . memoryDataError . Text.pack . show) . fmap parseData . Text.readFile

handleIOError :: (IOError -> IO a) -> IO a -> IO a
handleIOError = flip catchIOError

parseData :: Text -> Either Text (HashMap Text Int64)
parseData = fmap HashMap.fromList . parseFile

parseFile :: Text -> Either Text [(Text, Int64)]
parseFile = traverse parseLine . Text.lines

parseLine :: Text -> Either Text (Text, Int64)
parseLine l = case Text.words l of
  (pre : n : _)
    | Just pre' <- Text.stripSuffix ":" pre
    , Right (n', _) <- Text.decimal n -> Right (pre', n')
  _ -> memoryDataError $ "bad line: " <> l

memoryDataError :: Text -> Either Text a
memoryDataError x = Left $ "Pakej.Widget.Memory: " <> x
