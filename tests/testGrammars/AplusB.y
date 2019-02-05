{
module AplusB where

import ParserCombinator
}

%tokentype {Char}

%token a    {'a'}
%token b    {'b'}
%token plus {'+'}

%attribute {len}{Int}
%attribute {posn}{Int}

%%

Start :: {String}
  : a plus b -> {$1 ++ $2 ++ $3}

As
  : a b As -> {$1 : $2}
  |        -> {[]}
  | a{} b{} c -> {undefined}

{
und = undefined
}
