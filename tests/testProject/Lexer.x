{
module Lexer where
}

%wrapper "basic"

tokens :-
  "a" {\s -> A s}
  "b" {\s -> B s}
  "c" {\s -> C s}

{
data TokenABC = A String
              | B String
              | C String
}
