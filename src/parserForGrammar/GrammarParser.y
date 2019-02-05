{
module GrammarParser where

import GrammarLexer
import Grammar

}

%name       parseGrammarFromToken
%tokentype  { GrammarToken }
%error      { parseError }
%monad      { Either String }{ >>= }{ return }

%token    "%%"           { Tstartgrammar }
%token    "%token"       { Ttoken }
%token    "%tokentype"   { Ttokentype }
%token    "%attribute"   { Tattribute }
%token    ":"            { Tdef }
%token    "::"           { Ttypedef }
%token    "|"            { Talt }
%token    "->"           { Treturn }
%token    code           { Tcode $$ }
%token    word           { Tword $$ }

%nonassoc word
%nonassoc code
%left "|"

%%

Start :: { Grammar }
  : Code
    "%tokentype" Code
    TokenDefinitions
    Attributes
    "%%"
    NonterminalDefinitions
    EndedCode
    { Grammar{ startedCode = $1, tokenType = $3, tokens = $4, attributes = $5,
                         nonterminalDefinitions = $7, endedCode = $8 }}

EndedCode
  : Code { $1 }
  |      { "" }

TokenDefinitions :: {[(String, Code)]}
  : TokenDefinition TokenDefinitions   { $1 : $2 }
  |                                    { [] }

TokenDefinition :: {(String, Code)}
  : "%token" word Code                 { ($2, $3)}

Attributes :: {[(Code, Code, Code)]}
  : Attribute Attributes               { $1 : $2 }
  |                                    { [] }

Attribute :: {(Code, Code, Code)}
  : "%attribute" Code Code Code            { ($2, $3, $4) }

NonterminalDefinitions :: {[NonterminalDefinition]}
  : NonterminalDefinition NonterminalDefinitions { $1 : $2 }
  |                                              { [] }

NonterminalDefinition :: {NonterminalDefinition}
  : word ":" Rules
  {NonterminalDefinition {nameNonterminal = $1, signatureNonterminal = Nothing, rulesNonterminal = $3}}
  | word "::" Code ":" Rules
  {NonterminalDefinition {nameNonterminal = $1, signatureNonterminal = Just $3, rulesNonterminal = $5}}

Rules :: {[Rule]}
  : Rule RulesSeq  { $1 : $2 }

RulesSeq :: {[Rule]}
  : "|" Rule RulesSeq { $2 : $3 }
  |                   { [] }

Rule :: {Rule}
  : RuleUnits "->" Code {Rule ($1, $3)}

RuleUnits :: {[RuleUnit]}
  : word RuleUnits      { (RuleUnit $1 Nothing) : $2 }
  | word Code RuleUnits { (RuleUnit $1 (Just $2) : $3) }
  |                     { [] }

Code :: {Code}
  : code { unbracked $1 }

{
parseError = fail "Parse error"

unbracked :: String -> String
unbracked str = dropOpen $ dropClose str where
  dropOpen (s:ss) | s == '{' = ss
                  | otherwise = s:ss
  dropClose [s] | s == '}' = []
                | otherwise = [s]
  dropClose (s:ss) = s : dropClose ss
  dropClose [] = []

}
