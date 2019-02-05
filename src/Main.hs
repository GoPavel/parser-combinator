{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import GrammarParser (parseGrammarFromToken)
import GrammarLexer (alexScanTokens)
import ParserGenerator (codegeneration)

parseGrammar str = case parseGrammarFromToken $ alexScanTokens str of
  Right a -> a
  Left a -> error a

testInputFile =  "tests/testGrammars/Calc/Parser.y"
testOutputFile = "tests/testGrammars/Calc/Parser.hs"
debugPrintFile = "debug.info"

main ∷ IO()
main = do
  grammarFile <- readFile testInputFile
  writeFile debugPrintFile ("\n>>> grammarFilen\n" ++ grammarFile)
  let tokens = alexScanTokens grammarFile
  appendFile debugPrintFile ("\n>>> tokens\n" ++ show tokens)
  let grammar = either error id $ parseGrammarFromToken tokens
  appendFile debugPrintFile ("\n>>> grammar\n" ++ show grammar)
  codegeneration testOutputFile grammar
