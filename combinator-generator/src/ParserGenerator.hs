{-# LANGUAGE UnicodeSyntax #-}

module ParserGenerator(codegeneration) where

import Data.List
import Data.Function
import Grammar

import Format

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
    startNonterminal = (nameNonterminal . head) (nonterminalDefinitions grammar)

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
      ++ unwords ["parse_" ++ name, "= pSatisfy (\\case", format ["_"] code, "-> True; _ -> False) \n"]

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
               maybe "" (('{':) . (++"}") . format attrNames) code]

    generateHandleTerminal ∷ (String, Code) → String
    generateHandleTerminal (name, constr) | findUniqueDoubleDollar constr =
      "handleTerm (" ++ format ["s"] constr ++ ") = s"
                                          | otherwise = []

    generateParseFunction ∷ String
    generateParseFunction =
         "parseFull = runParserFull (parse_" ++ startNonterminal ++ " emptyAttr)\n"
      ++ "\n"
      ++ "parse = runParserEither (parse_" ++ startNonterminal ++ " emptyAttr)\n"
      ++ "\n"

-- addBracket ∷ String → String
-- addBracket str = "{" ++ str ++ "}"

findUniqueDoubleDollar ∷ String → Bool
findUniqueDoubleDollar (s1:s2:ss) | s1 == '$' && s2 == '$' = '$' `notElem` ss
                                  | otherwise = findUniqueDoubleDollar (s2:ss)
findUniqueDoubleDollar _ = False
