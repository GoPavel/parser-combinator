module Main where

import ParserABC
import LexerABC

main :: IO ()
main = do
  text <- getContents
  let tokens = alexScanTokens text
  print $ either id show $ parse tokens
