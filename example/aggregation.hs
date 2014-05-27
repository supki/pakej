{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Time (formatTime, getZonedTime)
import Pakej
import Prelude hiding ((.), id)
import System.Locale (defaultTimeLocale)

main :: IO ()
main = pakej $ private "status" . aggregate
  [ private "loadavg"  . text (loadavg "/proc/loadavg")
  , private "datetime" . text datetime
  ]

loadavg :: FilePath -> IO Text
loadavg = fmap (fromString . intercalate " → " . take 3 . words) . readFile

datetime :: IO Text
datetime = fmap format getZonedTime
 where format       = fromString . formatTime defaultTimeLocale formatString
       formatString = "%m.%d.%y, %a, %H:%M"
