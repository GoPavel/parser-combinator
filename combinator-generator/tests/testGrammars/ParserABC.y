{
module ABCParser (parse) where

import LexerABC
import ParserCombinator
}

%tokentype{TokenABC}

%token a { A $$ }
%token b { B $$ }
%token c { C $$ }

%attribute { len }{ Int }


%%

abcstring
   : alist blist{len = snd $1} clist{len = snd $1} -> { fst $1 ++ $2 ++ $3 }

alist :: { (String, Int) }
   : a alist -> { ($1 ++ fst $2, snd $2 + 1) }
   |           -> { ([], 0) }

blist :: { String }
   : b blist{len = len $$ - 1} -> { ($1 ++ $2)}
   |                           -> { if len $$ /= 0 then undefined else [] }

clist :: { String }
  : c clist{len = len $$ - 1} -> { ($1 ++ $2)}
  |                           -> { if len $$ /= 0 then undefined else [] }

{
happyError = error "parse error"
failUnless b msg = if b then () else error msg
}
