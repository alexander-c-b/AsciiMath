{
module Parser (parseAscii) where

import Prelude hiding (EQ, LT, GT)

import Ast
import Exception
import Lexer
}

%name parseAscii
%tokentype { (Token, Position) }
%monad { Either AsciimathException } { thenE } { \x -> Right x }

%token
  RAW         { (RAW _, _) }
  LETTER      { (LETTER _, _) }
  NUM         { (NUM _, _) }
  LDEL        { (LDEL _, _) }
  RDEL        { (RDEL _, _) }
  '/'         { (SLASH, _) }
  '_'         { (UNDERSCORE, _) }
  '^'         { (SUPER, _) }
  GREEK       { (GREEK _, _) }
  STDFUN      { (STDFUN _, _) }
  SQRT        { (SQRT, _) }
  TEXT        { (TEXT, _) }
  BB          { (BB, _) }
  BBB         { (BBB, _) }
  UCC         { (UCC, _) }
  TT          { (TT, _) }
  FR          { (FR, _) }
  SF          { (SF, _) }
  FRAC        { (FRAC, _) }
  ROOT        { (ROOT, _) }
  STACKREL    { (STACKREL, _) }
  ADD         { (ADD, _) }
  SUB         { (SUB, _) }
  MUL         { (MUL, _) }
  MMUL        { (MMUL, _) }
  MMMUL       { (MMMUL, _) }
  SSLASH      { (SSLASH, _) }
  BBSLASH     { (BBSLASH, _) }
  TIMES       { (TIMES, _) }
  DIV         { (DIV, _) }
  COMP        { (COMP, _) }
  OPLUS       { (OPLUS, _) }
  OTIMES      { (OTIMES, _) }
  ODOT        { (ODOT, _) }
  SUM         { (SUM, _) }
  PROD        { (PROD, _) }
  WEDGE       { (WEDGE, _) }
  WWEDGE      { (WWEDGE, _) }
  VV          { (VV, _) }
  VVV         { (VVV, _) }
  NN          { (NN, _) }
  NNN         { (NNN, _) }
  UU          { (UU, _) }
  UUU         { (UUU, _) }
  INT         { (INT, _) }
  OINT        { (OINT, _) }
  DEL         { (DEL, _) }
  GRAD        { (GRAD, _) }
  ADDSUB      { (ADDSUB, _) }
  VOID        { (VOID, _) }
  INFTY       { (INFTY, _) }
  ALEPH       { (ALEPH, _) }
  ANGLE       { (ANGLE, _) }
  THEREFORE   { (THEREFORE, _) }
  ABS         { (ABS, _) }
  CDOTS       { (CDOTS, _) }
  VDOTS       { (VDOTS, _) }
  DDOTS       { (DDOTS, _) }
  BSLASH      { (BSLASH, _) }
  QUAD        { (QUAD, _) }
  SPACE       { (SPACE, _) }
  AMPERSAND   { (AMPERSAND, _) }
  DOUBLESEMICOLON { (DOUBLESEMICOLON, _) }
  DIAMOND     { (DIAMOND, _) }
  SQUARE      { (SQUARE, _) }
  LFLOOR      { (LFLOOR, _) }
  RFLOOR      { (RFLOOR, _) }
  LCEIL       { (LCEIL, _) }
  RCEIL       { (RCEIL, _) }
  CC          { (CC, _) }
  ENSNN       { (ENSNN, _) }
  QQ          { (QQ, _) }
  RR          { (RR, _) }
  ZZ          { (ZZ, _) }
  EQ          { (EQ, _) }
  NEQ         { (NEQ, _) }
  LT          { (LT, _) }
  GT          { (GT, _) }
  LE          { (LE, _) }
  GE          { (GE, _) }
  PREC        { (PREC, _) }
  SUCC        { (SUCC, _) }
  IN          { (IN, _) }
  NOTIN       { (NOTIN, _) }
  SUBSET      { (SUBSET, _) }
  SUPSET      { (SUPSET, _) }
  SUBSETE     { (SUBSETE, _) }
  SUPSETE     { (SUPSETE, _) }
  MOD         { (MOD, _) }
  CONGR       { (CONGR, _) }
  APPROX      { (APPROX, _) }
  PROP        { (PROP, _) }
  AND         { (AND, _) }
  OR          { (OR, _) }
  NOT         { (NOT, _) }
  IMPLIES     { (IMPLIES, _) }
  IF          { (IF, _) }
  IFF         { (IFF, _) }
  FORALL      { (FORALL, _) }
  EXISTS      { (EXISTS, _) }
  FALSUM      { (FALSUM, _) }
  TAUT        { (TAUT, _) }
  TURNSTILE   { (TURNSTILE, _) }
  TTURNSTILE  { (TTURNSTILE, _) }
  UARR        { (UARR, _) }
  DARR        { (DARR, _) }
  LARR        { (LARR, _) }
  TO          { (TO, _) }
  MAPSTO      { (MAPSTO, _) }
  HARR        { (HARR, _) }
  LLARR       { (LLARR, _) }
  TILDE       { (TILDE, _) }
  HAT         { (HAT, _) }
  BAR         { (BAR, _) }
  UL          { (UL, _) }
  VEC         { (VEC, _) }
  DOTOP       { (DOTOP, _) }
  DDOT        { (DDOT, _) }
  COMMA       { (COMMA, _) }
  DOT         { (DOT, _) }
  SEMICOLON   { (SEMICOLON, _) }
  QUOTE       { (QUOTE, _) }
  FACTO       { (FACTO, _) }

%%

code:
                { [] }
    | expr code { $1 : $2 }

expr:
    simpleExpr                                 { Simple $1 }
    | simpleExpr '/' simpleExpr                { Frac $1 $3 }
    | simpleExpr '_' simpleExpr                { Under $1 $3 }
    | simpleExpr '^' simpleExpr                { Super $1 $3 }
    | simpleExpr '_' simpleExpr '^' simpleExpr { SubSuper $1 $3 $5 }

const:
    LETTER        { let (LETTER s, _) = $1 in Letter s }
    | NUM         { let (NUM n,    _) = $1 in Number n }
    | GREEK       { let (GREEK s,  _) = $1 in GreekLetter s }
    | STDFUN      { let (STDFUN s, _) = $1 in StdFun s }
    -- Operation symbols
    | ADD         { Add }
    | SUB         { Sub }
    | MUL         { Mul }
    | MMUL        { Mmul }
    | MMMUL       { Mmmul }
    | SSLASH      { Sslash }
    | BBSLASH     { Bbslash }
    | TIMES       { Times }
    | DIV         { Div }
    | COMP        { Comp }
    | OPLUS       { Oplus }
    | OTIMES      { Otimes }
    | ODOT        { Odot }
    | SUM         { Sum }
    | PROD        { Prod }
    | WEDGE       { Wedge }
    | WWEDGE      { Wwedge }
    | VV          { Vv }
    | VVV         { Vvv }
    | NN          { Nn }
    | NNN         { Nnn }
    | UU          { Uu }
    | UUU         { Uuu }
    -- Miscellaneous symbols
    | INT         { Inte }
    | OINT        { Oint }
    | DEL         { Del }
    | GRAD        { Grad }
    | ADDSUB      { Addsub }
    | VOID        { Void }
    | INFTY       { Infty }
    | ALEPH       { Aleph }
    | ANGLE       { Angle }
    | THEREFORE   { Therefore }
    | ABS         { Abs }
    | CDOTS       { Cdots }
    | VDOTS       { Vdots }
    | DDOTS       { Ddots }
    | BSLASH      { Bslash }
    | QUAD        { Quad }
    | SPACE       { Space }
    | AMPERSAND   { Ampersand }
    | DOUBLESEMICOLON { DoubleSemicolon }
    | DIAMOND     { Diamond }
    | SQUARE      { Square }
    | LFLOOR      { Lfloor }
    | RFLOOR      { Rfloor }
    | LCEIL       { Lceil }
    | RCEIL       { Rceil }
    | CC          { Cc }
    | ENSNN       { Ensnn }
    | QQ          { Qq }
    | RR          { Rr }
    | ZZ          { Zz }
    -- Relation symbols
    | EQ          { Eq }
    | NEQ         { Neq }
    | LT          { Lt }
    | GT          { Gt }
    | LE          { Le }
    | GE          { Ge }
    | PREC        { Prec }
    | SUCC        { Succ }
    | IN          { In }
    | NOTIN       { Notin }
    | SUBSET      { Subset }
    | SUPSET      { Supset }
    | SUBSETE     { Subsete }
    | SUPSETE     { Supsete }
    | MOD         { Mod }
    | CONGR       { Congr }
    | APPROX      { Approx }
    | PROP        { Prop }
    -- Logical symbols
    | AND         { And }
    | OR          { Or }
    | NOT         { Not }
    | IMPLIES     { Implies }
    | IF          { If }
    | IFF         { Iff }
    | FORALL      { Forall }
    | EXISTS      { Exists }
    | FALSUM      { Falsum }
    | TAUT        { Taut }
    | TURNSTILE   { Turnstile }
    | TTURNSTILE  { Tturnstile }
    -- Arrows
    | UARR        { Uarr }
    | DARR        { Darr }
    | LARR        { Larr }
    | TO          { To }
    | MAPSTO      { Mapsto }
    | HARR        { Harr }
    | LLARR       { Llarr }
    -- Additionnal tokens
    | COMMA       { Comma }
    | DOT         { Dot }
    | SEMICOLON   { Semicolon }
    | QUOTE       { Quote }
    | FACTO       { Facto }

op1:
    SQRT        { Usqrt }
    | TEXT      { Utext }
    | BB        { Ubb }
    | BBB       { Ubbb }
    | UCC       { Ucc }
    | TT        { Utt }
    | FR        { Ufr }
    | SF        { Usf }
    | TILDE     { Utilde }
    | HAT       { Uhat }
    | BAR       { Ubar }
    | UL        { Uul }
    | VEC       { Uvec }
    | DOTOP     { Udot }
    | DDOT      { Uddot }

op2:
    FRAC        { BFrac }
    | ROOT      { BRoot }
    | STACKREL  { BStackRel }

lDel : LDEL     { let (LDEL s, _) = $1 in ldel s }
rDel : RDEL     { let (RDEL s, _) = $1 in rdel s }

simpleExpr:
    const                       { SEConst $1 }
    | lDel code rDel            { Delimited $1 $2 $3 }
    | op1 simpleExpr            { UnaryApp $1 $2 }
    | op2 simpleExpr simpleExpr { BinaryApp $1 $2 $3 }
    | RAW                       { let (RAW s, _) = $1 in Raw s }

{

thenE :: Either AsciimathException a -> (a -> Either AsciimathException b) -> Either AsciimathException b
thenE (Left err) _ = Left err
thenE (Right x) f = f x

happyError tokens =
  let (tok, pos) = head tokens in
  Left $ LexicalError (show tok) pos

-- Conversion
rdel :: String -> RBracket
rdel ")" = RPar
rdel "]" = RCro
rdel "}" = RBra
rdel ":)" = RChe
rdel ":}" = RBraCons

ldel :: String -> LBracket
ldel "(" = LPar
ldel "[" = LCro
ldel "{" = LBra
ldel "(:" = LChe
ldel "{:" = LBraCons
}
