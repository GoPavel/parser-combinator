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
   : alist blist{len = snd $1} clist{len = snd $1} -> { (fst $1 ++) `fmap` $ (liftA2 (++)) $2  $3 }

alist
   : a alist -> { ($1 ++ fst $2, snd $2 + 1) }
   |           -> { ([], 0) }

blist
   : b blist{len = len $$ - 1} -> { fmap ($1++) $2}
   |                           -> { if len $$ /= 0 then Nothing else (Just []) }

clist
  : c clist{len = len $$ - 1} -> { fmap ($1 ++) $2}
  |                           -> { if len $$ /= 0 then Nothing else (Just []) }
