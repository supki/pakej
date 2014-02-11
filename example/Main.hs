{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import System.Command.QQ (sh)
import Prelude hiding ((.), id)

import Pakej


main :: IO ()
main = pakej $ aggregate
  [ public "date" . text [sh| date +"%m.%d.%y, %a, %H:%M:%S" |]
  ]
