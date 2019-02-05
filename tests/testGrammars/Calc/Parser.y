{
module Parser where

import Lexer
}

%tokentype { TokenCalc }

%token val   { Tval $$ }
%token plus  { Tplus }
%token minus { Tminus }
%token pow   { Tpow }
%token open  { Topen }
%token close { Tclose }
%token mul   { Tmul }

%attribute { acc_op } { Integer -> Integer } { id }

%%

E :: { Integer }
  : M { acc_op = acc_op $$ } plus E  { acc_op = ($1 +)} ->  { $3 }
  | M { acc_op = acc_op $$ } minus E { acc_op = ($1 -)} ->  { $3 }
  | M { acc_op = acc_op $$ }                            ->  { $1 }

M :: { Integer }
  : P {acc_op = acc_op $$} mul M { acc_op = ($1 *) } -> { $3 }
  | P {acc_op = acc_op $$}                           -> { $1 }

P :: { Integer }
  : T {acc_op = acc_op $$} pow P  -> { $1 ^ $3 }
  | T {acc_op = acc_op $$}        -> { $1 }

T :: { Integer }
  : val           -> { (acc_op $$) $1 }
  | open E close  -> { (acc_op $$) $2 }
