{-# LANGUAGE UnicodeSyntax #-}

module Format(format) where

import Data.List
import ParserCombinator
import Control.Applicative

format = flip format'

format' ∷ String → [String] → String
format' (s:ss) xs | s == '$' = case head $ runP (parseDigit xs) ss of
  (val, ss') -> xs !! val ++ format' ss' xs
                 | otherwise = s : format' ss xs
format' [] xs = []

parseDigit ∷ [String] → Parser Char Int
parseDigit name  = my_fold <$> some pDigitAsInt
               <|> const 0 <$> pDollar where
  my_fold ∷ [Int] → Int
  my_fold = foldr (\x acc -> acc * 10 + x) 0 . reverse

pDollar :: Parser Char Char
pDollar = pSatisfy (=='$')

pDigit :: Parser Char Char
pDigit = pSatisfy (\ch -> fromEnum ch >= fromEnum '0' && fromEnum ch <= fromEnum '9')

pDigitAsInt ∷ Parser Char Int
pDigitAsInt = (\x -> fromEnum x - fromEnum '0') <$> pDigit
