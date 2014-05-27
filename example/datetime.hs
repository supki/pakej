{-# LANGUAGE OverloadedStrings #-}

import Data.String (IsString(..))
import Data.Text (Text)
import Data.Time (formatTime, getZonedTime)
import Pakej
import Prelude hiding ((.), id)
import System.Locale (defaultTimeLocale)

main :: IO ()
main = pakej $ private "datetime" . text datetime

datetime :: IO Text
datetime = fmap format getZonedTime
 where format       = fromString . formatTime defaultTimeLocale formatString
       formatString = "%m.%d.%y, %a, %H:%M"
