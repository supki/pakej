{-# LANGUAGE CPP #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.String (fromString)
import           Data.Text (Text)
import           Pakej
import qualified Pakej.Widget.Cpu as Cpu
import           Prelude hiding ((.), id)
import           Text.Printf (printf)

#ifndef __NUM_CORES__
#define __NUM_CORES__ 2
#endif

main :: IO ()
main = pakej $ private "all" . aggregate
  (map (\n -> private (cpuid n) . cpu n) (Nothing : map Just [0 .. __NUM_CORES__ - 1]))

cpuid :: Maybe Int -> Text
cpuid n = fromString ("cpu" ++ maybe " " show n)

cpu :: Maybe Int -> PakejWidget Text
cpu = fmap (fromString . printf "%2.f%%") . Cpu.widget
