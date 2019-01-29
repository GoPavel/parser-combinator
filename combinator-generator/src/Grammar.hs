{-# LANGUAGE UnicodeSyntax #-}

module Grammar where

type Code = String
type Name = String

data RuleUnit = RuleUnit Name (Maybe Code) deriving (Show)

newtype Rule = Rule ([RuleUnit], Code) deriving (Show)

data NonterminalDefinition = NonterminalDefinition {
  nameNonterminal ∷ Name,
  signatureNonterminal ∷ Maybe String,
  rulesNonterminal ∷ [Rule]
} deriving (Show)

data Grammar = Grammar {
  startedCode ∷ Code,
  tokenType ∷ Code,
  tokens ∷ [(Name, Code)],
  attributes ∷ [(Code, Code, Code)],
  nonterminalDefinitions ∷ [NonterminalDefinition],
  endedCode ∷ Code
} deriving (Show)
