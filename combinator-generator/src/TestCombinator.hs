{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}

module TestCombinator where

import Prelude hiding ((<*>), (<|>), (<$>), (<$), (<*), (*>))
import Data.Function

data Token = Identifier
           | Ident String
           | Number Int
           | IfSymbol
           | ThenSymbol
  deriving(Show)

instance Eq Token where
  (Ident _) == Identifier = True
  _ == _ = False

newtype Parser s t = Parser {runP :: [s] → [(t, [s])]}

pLettera ∷ Parser Char Int
pLettera = Parser { runP =
  \case
    (x:xs) | x == 'a' -> [(0, xs)]
    _ -> []
}

pSym ∷ (Eq s) ⇒ s → Parser s s
pSym a = Parser { runP =
  \case
    (x:xs) | x == a -> [(x, xs)]
    _ -> []
}

pIdent ∷ Parser Token Token
pIdent = pSym Identifier

pReturn ∷ a → Parser s a
pReturn a = Parser { runP =
  \inp -> [(a, inp)]
}

pFail ∷ Parser s t
pFail = Parser { runP = const []}

infixl 5 <*>
(<*>) ∷ Parser s (b → a) → Parser s b → Parser s a
Parser{runP = p1} <*> Parser{runP = p2} = Parser { runP =
  \inp -> [(v1 v2, ss2) | (v1, ss1) <- p1 inp
                        , (v2, ss2) <- p2 ss1]
}

pStringaa ∷ Parser Char [Int]
pStringaa = pReturn (:) <*> pLettera <*> (pReturn (:[]) <*> pLettera)

infixr 3 <|>
(<|>) ∷ Parser s a → Parser s a → Parser s a
Parser{runP=p1} <|> Parser{runP=p2} = Parser { runP =
  \inp -> p1 inp ++ p2 inp
}

pChoice ∷ [Parser s a] → Parser s a
pChoice = foldr (<|>) pFail

-- S -> (S) S | e
parens ∷ Parser Char Int
parens =  pReturn (\_ x _ y -> (x+1) `max` y)
      <*> pSym '(' <*> parens <*> pSym ')' <*> parens
  <|> pReturn 0

infix 7 <$>
(<$>) ∷ (a → b) → Parser s a → Parser s b
f <$> p = pReturn f <*> p

-- S -> (S) S | e
parens' ∷ Parser Char Int
parens' =  (\_ x _ y -> (x+1) `max` y)
      <$> pSym '(' <*> parens' <*> pSym ')' <*> parens'
  <|> pReturn 0

infixl 3 <?>
infixl 5 <*, *>
infixl 7 <$

(<$) ∷ (a → b) → Parser s t → Parser s (a -> b)
f <$ p = const <$> pReturn f <*> p
(<*) ∷ Parser s t1 → Parser s t2 → Parser s t1
p <* q = const <$> p <*> q
(*>) ∷ Parser s t1 → Parser s t2 → Parser s t2
p *> q = id <$ p <*> q

(<?>) ∷ Parser s a → a → Parser s a
p <?> v = p <|> pReturn v

parens'' = (max . (1+)) <$> (pSym '(' *> parens'' <* pSym ')') <*> parens'' <?> 0

pSyms ∷ Eq s ⇒ [s] → Parser s [s]
pSyms [] = pReturn []
pSyms (x:xs) = (:) <$> pSym x <*> pSyms xs

pSatisfy ∷ (s → Bool) → Parser s s
pSatisfy p = Parser { runP =
  \case
    (x:xs) | p x -> [(x, xs)]
    _  -> []
}

pMany ∷ Parser s a → Parser s [a]
pMany p = (:) <$> p <*> pMany p <?> []
pMany1 ∷ Parser s a → Parser s [a]
pMany1 p = (:) <$> p <*> pMany p

applyAll ∷ a → [a → a] → a
applyAll = foldl (&)

pChainL ∷ Parser s (a → a → a) → Parser s a → Parser s a
pChainL op p = applyAll <$> p <*> pMany (flip <$> op <*> p)

infixl 5 <**>
(<**>) ∷ Parser s b → Parser s (b → a) → Parser s a
p1 <**> p2 = (&) <$> p1 <*> p2

infixl 3 <??>
(<??>) ∷ Parser s a → Parser s (a → a) → Parser s a
p <??> q = p <**> (q <?> id)

pChainR ∷ Parser s (a → a → a) → Parser s a → Parser s a
pChainR op p = p <??> (flip <$> op <*> pChainR op p)

pOp ∷ (a → b → c, String) → Parser Char (a → b → c)
pOp (sem, str) = sem <$ pSyms str

anyOp ∷ [(a → b → c, String)] → Parser Char (a → b → c)
anyOp = pChoice . map pOp

addops = anyOp [((+), "+"), ((-), "-")]
pPlusMinusTimes = foldr pChainL pInteger [addops, mulops]
mulops = anyOp [((*), "*")]
pDigit = pSatisfy (\ch ->  ch >= '0' && ch <= '9')
pDigitAsInt = (\ch -> fromEnum ch - fromEnum '0') <$> pDigit
pInteger = foldl (\acc e -> 10*acc + e) 0 <$> pMany1 pDigitAsInt

pPack ∷ Eq s ⇒ [s] → Parser s a → [s] → Parser s a
pPack open p close = pSyms open *> p <* pSyms close

pExpr = foldr pChainL pFactor [addops, mulops]
pFactor = pInteger <|> pPack "(" pExpr ")"
