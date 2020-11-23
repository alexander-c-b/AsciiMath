module Main (main) where

import Control.Exception as E
import Text.Pandoc.JSON
import AsciiMath
import Data.Text (pack,unpack)


main :: IO ()
main = E.catch (toJSONFilter asciimath) printAndExit
  where asciimath (Math t s) = Math t (pack $ run $ unpack s)
        asciimath x = x
