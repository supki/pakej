-- | This is a template pakej.hs file, provided with
-- the intention to be customized by Pakej users
{-# LANGUAGE OverloadedStrings #-} -- This extension is not required, but users will probably need it
module Main (main) where

-- All Pakej functionality is exported by this single module
import Pakej
-- We need (Category..) and Category.id—exported from Pakej module—to compose widgets
import Prelude hiding ((.), id)

-- This is not very interesting widget (it does exactly nothing) but it's the basis for extension
main :: IO ()
main = pakej id
