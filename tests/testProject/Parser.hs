{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative hiding ((<**>))
import Data.Function
import ParserCombinator
-- import Lexer

-- satisfyA ∷ TokenABC → Bool
-- satisfyA (A _) = True
-- satisfyA _ = False


data TokenABC = A String
              | B String
              | C String

pA ∷ Parser TokenABC TokenABC
pA = pSatisfy (\case (A _) -> True)
pB ∷ Parser TokenABC TokenABC
pB = pSatisfy (\case (B _) -> True)
pC ∷ Parser TokenABC TokenABC
pC = pSatisfy (\case (C _) -> True)

data Attr = Attr {len :: Int}

emptyAttr ∷ Attr
emptyAttr = Attr{len = undefined}

abcstring ∷ Parser TokenABC [TokenABC]
abcstring = do
  t1 ← alist undefined
  t2 ← blist emptyAttr{len = snd t1}
  t3 ← clist emptyAttr{len = snd t1}
  return $ fst t1 ++ t2 ++ t3

alist ∷ Attr → Parser TokenABC ([TokenABC], Int)
alist attr =
  do
    t1 ← pA
    t2 ← alist emptyAttr
    return $ (t1 : fst t2, snd t2 + 1)
  <|>
  do
    return $ ([], 0)

blist ∷ Attr → Parser TokenABC [TokenABC]
blist attr =
  do
    t1 ← pB
    t2 ← blist emptyAttr{len = len attr - 1}
    return $ t1 : t2
  <|>
  do
    return $ if len attr /= 0 then undefined else []


clist ∷ Attr → Parser TokenABC [TokenABC]
clist attr =
  do
    t1 ← pC
    t2 ← clist emptyAttr{len = len attr - 1}
    return $ t1 : t2
  <|>
  do
    return $ if len attr /= 0 then undefined else []
