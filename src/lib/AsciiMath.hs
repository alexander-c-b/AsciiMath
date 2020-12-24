module AsciiMath (readAscii, writeTeX, compile, run, AsciimathException(..), renderError, printAndExit) where

import Control.Exception (throw)

import Ast                           (Code)
import Exception                     (AsciimathException(ErrorAndSource)
                                     ,printAndExit,renderError)
import Lexer                         (get_tokens)
import Parser                        (parseAscii)
import Passes                        (passes)
import TeXWriter                     (writeTeX)

readAscii :: String -> Either AsciimathException Code
readAscii s = return . passes =<< parseAscii =<< get_tokens s

compile :: String -> Either AsciimathException String
compile s = fmap writeTeX $ readAscii s

run :: String -> String
run s = case compile s of
  Right txt -> txt
  Left e -> throw $ ErrorAndSource e s
