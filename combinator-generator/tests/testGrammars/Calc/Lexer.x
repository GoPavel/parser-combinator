{
module Lexer where
}

%wrapper "basic"


tokens :-
  $white+  ;
  "+"      {\_ -> Tplus }
  "-"      {\_ -> Tminus }
  "*"      {\_ -> Tmul }
  "^"      {\_ -> Tpow }
  "("      {\_ -> Topen }
  ")"      {\_ -> Tclose }
  [0-9]+   {\s -> Tval (read s :: Integer)}


{

data TokenCalc =
    Tval Integer
  | Tplus
  | Tminus
  | Tmul
  | Tpow
  | Topen
  | Tclose
  deriving (Show, Eq)
}
