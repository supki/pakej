{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import System.Command.QQ (sh)

import Pakej


main :: IO ()
main = pakej
  [ run $ "date" ~> [sh| date +"%m.%d.%y, %a, %H:%M %p" |]
  ]
