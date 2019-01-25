{-# LANGUAGE UnicodeSyntax #-}

module Grammar(codegeneration) where

import Data.List
import Util
import Data.Function

import Format

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
  tokens ∷ [(String, Code)],
  attributes ∷ [(Code, Code)],
  nonterminalDefinitions ∷ [NonterminalDefinition],
  endedCode ∷ Code
} deriving (Show)

codegeneration ∷ FilePath → Grammar → IO()
codegeneration filePath grammar = do
  writeFile' ""
  appendFile' "{-# LANGUAGE LambdaCase #-}\n"
  appendFile' $ startedCode grammar
  appendFile' "import Control.Applicative\n"
  appendFile' "\n"
  generateAttrDefinition (attributes grammar)
  appendFile' "\n"
  appendFile' $ intercalate "\n" $ map generateTokenSatisfy (tokens grammar)
  appendFile' "\n"
  appendFile' $ intercalate "\n\n" $ map generateNonterminalDefinition (nonterminalDefinitions grammar)
  appendFile' "\n"
  appendFile' "\n"
  appendFile' $ intercalate "\n" $ map generateHandleTerminal (tokens grammar)
  appendFile' "\n"
  appendFile' "handleTerm term = undefined\n"
  appendFile' "\n"
  appendFile' generateParseFunction
  appendFile' "\n"
  appendFile' (endedCode grammar)
  where
    writeFile' = writeFile filePath
    appendFile' = appendFile filePath
    tokenType' = tokenType grammar

    terminals ∷ [String]
    terminals = map fst (tokens grammar)

    generateAttrDefinition ∷ [(Code, Code)] → IO()
    generateAttrDefinition [] = return ()
    generateAttrDefinition attrs = do
      appendFile' "data Attr = Attr {\n"
      appendFile' $ intercalate ",\n" $ map (\(n, t) -> "  " ++ n ++ " :: " ++ t) attrs
      appendFile' "\n"
      appendFile' "}\n"
      appendFile' "emptyAttr :: Attr\n"
      appendFile' $ "emptyAttr = Attr {"
                ++  intercalate ", " (map (\(n, t) -> n ++ " = error \"undefined attr\"") attrs)
                ++  "}"
      appendFile' "\n"


    generateTokenSatisfy ∷ (String, Code) → String
    generateTokenSatisfy (name, code) =
         unwords ["parse_" ++ name, ":: Parser", tokenType', tokenType', "\n"]
      ++ unwords ["parse_" ++ name, "= pSatisfy (\\case", format ["_"] code, "-> True) \n"]

    generateNonterminalDefinition ∷ NonterminalDefinition → String
    generateNonterminalDefinition NonterminalDefinition {nameNonterminal = name,
                                                         rulesNonterminal = rules,
                                                         signatureNonterminal = sign} =
          maybe "" (unwords . (\sign -> ["parse_" ++ name, ":: Attr -> Parser", tokenType', sign, "\n"])) sign
      ++  "parse_" ++ name ++ " attr =\n"
      ++  intercalate "\n  <|>\n" (map (("  do\n" ++ ) . generateRule) rules)

    attrNames ∷ [String]
    attrNames = "attr" : map (\i -> "t" ++ show i) [1..]

    generateRule ∷ Rule → String
    generateRule (Rule (units, code)) = intercalate "\n" $ map (\s ->  "    " ++ s) $ filter (not . null ) $
         zipWith generateUnit [1..] units
      ++ ["return $ " ++ format attrNames code]

    generateUnit ∷ Int → RuleUnit → String
    generateUnit i (RuleUnit name code) | name `elem` terminals =
         unwords [attrNames !! i ++ "'", "<-", "parse_" ++ name, "\n"]
      ++ "    let " ++ attrNames !! i ++ " = handleTerm " ++ attrNames !! i ++ "'"
                                        | otherwise =
      unwords [attrNames !! i,
               "<-",
               "parse_" ++ name,
               "emptyAttr",
               maybe "" (bracket . format attrNames) code]

    generateHandleTerminal ∷ (String, Code) → String
    generateHandleTerminal (name, constr) | findDD constr =
      "handleTerm (" ++ format ["s"] constr ++ ") = s"
                                          | otherwise = []

    generateParseFunction ∷ String
    generateParseFunction =
      "parse tokens = parse_" ++ (nameNonterminal . head) (nonterminalDefinitions grammar) ++ " tokens\n"

bracket ∷ String → String
bracket str = "{" ++ str ++ "}"

findDD ∷ String → Bool
findDD (s1:s2:ss) | s1 == '$' && s2 == '$' = '$' `notElem` ss
                  | otherwise = findDD (s2:ss)
findDD _ = False
