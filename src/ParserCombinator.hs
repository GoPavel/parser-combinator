{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}

module ParserCombinator where

import Control.Applicative hiding ((<**>))
import Data.Function

newtype Parser s t = Parser { runP ∷ [s] → [(t, [s])] }

runParserEither ∷ Parser s t → [s] → Either String t
runParserEither p inp = case runParserFull p inp of
  [] -> Left "parse error"
  x:_ -> Right x

runParserFull ∷ Parser s t → [s] → [t]
runParserFull p inp = map fst $ takeWhile (\(t, inp') -> null inp') $ runP p inp

pFail ∷ Parser s t
pFail =  Parser { runP = const [] }

pSatisfy ∷ (s → Bool) → Parser s s
pSatisfy p = Parser {runP =
  \case
    (x:xs) | p x -> [(x, xs)]
    _ -> []
}

instance Functor (Parser s) where
  f `fmap` p = pure f <*> p
  f <$ p = const <$> pure f <*> p

instance Applicative (Parser s) where
  pure a = Parser { runP = \inp -> [(a, inp)] }
  Parser{runP = runP1} <*> Parser{runP = runP2} = Parser { runP =
    \inp -> [(v1 v2, inp'') | (v1, inp') <- runP1 inp
                            , (v2, inp'') <- runP2 inp']
  }
  p <* q = const <$> p <*> q
  p *> q = id <$ p <*> q


instance Alternative (Parser s) where
  Parser{runP=p1} <|> Parser{runP=p2} = Parser { runP =
    \inp -> p1 inp ++ p2 inp
  }
  empty =  Parser { runP = const [] }
  many p = (:) <$> p <*> many p <|> pure []
  some p = (:) <$> p <*> many p

infixl 4 <?>, <**>, <??>
(<?>) ∷ Parser s a → a → Parser s a
p <?> v = p <|> pure v

(<**>) ∷ Parser s b → Parser s (b → a) → Parser s a
p1 <**> p2 = (&) <$> p1 <*> p2

(<??>) ∷ Parser s a → Parser s (a → a) → Parser s a
p <??> q = p <**> (q <?> id)

instance Monad (Parser s) where
  return = pure
  p >>= v2p = Parser { runP =
    \inp -> [ (v2, inp'') | (v1, inp') <- runP p inp
                          , (v2, inp'') <- runP (v2p v1) inp']
  }

-- data Token = Plus | Minus | Name String
--
-- pTokenPlus ∷ Parser Token Token
-- pTokenPlus = Parser { runP =
--   \case
--     (Plus:xs) -> [(Plus, xs)]
--     _ -> []
-- }
--
-- pTokenMinus ∷ Parser Token Token
-- pTokenMinus = Parser { runP =
--   \case
--     (Minus:xs) -> [(Minus, xs)]
--     _ -> []
-- }
--
-- pTokenName ∷ Parser Token Token
-- pTokenName = Parser { runP =
--   \case
--     (Name a:xs) -> [(Name a, xs)]
--     _ -> []
-- }
