{
  module GrammarLexer where
}

%wrapper "basic"

@word = [a-zA-z][a-zA-z0-9']*
@code = "{"([^\{\}]|[\n])*"}"

tokens :-
  $white+           ;
  "%%"              {\_ -> Tstartgrammar }
  "%token"          {\_ -> Ttoken }
  "%tokentype"      {\_ -> Ttokentype }
  "%attribute"      {\_ -> Tattribute }
  "::"              {\_ -> Ttypedef}
  ":"               {\_ -> Tdef }
  "|"               {\_ -> Talt }
  "->"              {\_ -> Treturn }
  @code             {\s -> Tcode s }
  @word             {\s -> Tword s }
{

data GrammarToken = Tstartgrammar
                  | Ttoken
                  | Ttokentype
                  | Tattribute
                  | Ttypedef
                  | Tdef
                  | Talt
                  | Treturn
                  | Tcode String
                  | Tword String
  deriving(Eq, Show)
}
