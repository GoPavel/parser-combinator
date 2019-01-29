module Main where

import Parser
import Lexer

printResult :: String -> String
printResult text = either id show $ parse $ alexScanTokens text

main :: IO ()
main = do
  print "Hello, world"
