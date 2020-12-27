-- vim: set foldmethod=marker:
-- Module and imports {{{1
{
module Parser (parseAscii) where

import Prelude hiding (EQ, LT, GT)

import Ast
import Exception
import Lexer
}

-- Preliminaries {{{1
%name parseAscii
%tokentype { (Token, Position) }
%monad { Either AsciimathException } { thenE } { \x -> Right x }

%token
  RAW         { (RAW _, _) }
  LETTERS     { (LETTERS _, _) }
  DIFF        { (DIFF _, _) }
  NUM         { (NUM _, _) }
  LDEL        { (LDEL _, _) }
  RDEL        { (RDEL _, _) }
  '/'         { (SLASH, _) }
  '_'         { (UNDERSCORE, _) }
  '^'         { (SUPER, _) }
  '&'         { (AMPERSAND, _) }
  ';;'        { (DOUBLESEMICOLON, _) }
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
  SMALLSPACE  { (SMALLSPACE, _) }
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

-- Code, matrices, exprs {{{1
code :: { Code }
code :  matrix { Matrix $1 }
     |  exprs  { Exprs $1 }

matrix :: { [[[Expr]]] }
matrix :  exprs      '&'  matrixCols { [$1 : $3] }
       |  matrixCols ';;' matrixRows { $1 : $3 }

matrixCols :: { [[Expr]] }
matrixCols :  exprs                { [$1] }
           |  exprs '&' matrixCols { $1 : $3 }

matrixRows :: { [[[Expr]]] }
matrixRows :  matrixCols                 { [$1] }
           |  matrixCols ';;' matrixRows { $1 : $3 }

exprs :: { [Expr] }
exprs :                  { [] }
      |  expr spaceExprs { $1 : $2 }

spaceExprs :: { [Expr] }
spaceExprs :                  { [] }
           |  expr spaceExprs { potentialSpace $1 ($1 : $2) }

-- Expressions {{{1
expr :: { Expr }
expr :  simpleExpr                               { Simple $1 }
     |  simpleExpr '/' simpleExpr                { Frac $1 $3 }
     |  simpleExpr '_' simpleExpr                { Under $1 $3 }
     |  simpleExpr '^' simpleExpr                { Super $1 $3 }
     |  simpleExpr '_' simpleExpr '^' simpleExpr { SubSuper $1 $3 $5 }

-- Simple Expressions {{{1
simpleExpr :: { SimpleExpr }
simpleExpr :  const                     { SEConst $1 }
           |  lDel code rDel            { Delimited $1 $2 $3 }
           |  op1 simpleExpr            { UnaryApp $1 $2 }
           |  op2 simpleExpr simpleExpr { BinaryApp $1 $2 $3 }
           |  RAW                       { let (RAW s, _) = $1 in Raw s }

-- Constants {{{1
const :: { Constant }
const :  LETTERS     { let (LETTERS s, _) = $1 in Letters s }
      |  DIFF        { let (DIFF s,    _) = $1 in Diff s }
      |  NUM         { let (NUM n,     _) = $1 in Number n }
      |  GREEK       { let (GREEK s,   _) = $1 in GreekLetter s }
      |  STDFUN      { let (STDFUN s,  _) = $1 in StdFun s }
      -- Operation symbols
      |  ADD         { Add }
      |  SUB         { Sub }
      |  MUL         { Mul }
      |  MMUL        { Mmul }
      |  MMMUL       { Mmmul }
      |  SSLASH      { Sslash }
      |  BBSLASH     { Bbslash }
      |  TIMES       { Times }
      |  DIV         { Div }
      |  COMP        { Comp }
      |  OPLUS       { Oplus }
      |  OTIMES      { Otimes }
      |  ODOT        { Odot }
      |  SUM         { Sum }
      |  PROD        { Prod }
      |  WEDGE       { Wedge }
      |  WWEDGE      { Wwedge }
      |  VV          { Vv }
      |  VVV         { Vvv }
      |  NN          { Nn }
      |  NNN         { Nnn }
      |  UU          { Uu }
      |  UUU         { Uuu }
      -- Miscellaneous symbols
      |  INT         { Inte }
      |  OINT        { Oint }
      |  DEL         { Del }
      |  GRAD        { Grad }
      |  ADDSUB      { Addsub }
      |  VOID        { Void }
      |  INFTY       { Infty }
      |  ALEPH       { Aleph }
      |  ANGLE       { Angle }
      |  THEREFORE   { Therefore }
      |  ABS         { Abs }
      |  CDOTS       { Cdots }
      |  VDOTS       { Vdots }
      |  DDOTS       { Ddots }
      |  BSLASH      { Bslash }
      |  QUAD        { Quad }
      |  SPACE       { Space }
      |  SMALLSPACE  { SmallSpace }
      |  DIAMOND     { Diamond }
      |  SQUARE      { Square }
      |  LFLOOR      { Lfloor }
      |  RFLOOR      { Rfloor }
      |  LCEIL       { Lceil }
      |  RCEIL       { Rceil }
      |  CC          { Cc }
      |  ENSNN       { Ensnn }
      |  QQ          { Qq }
      |  RR          { Rr }
      |  ZZ          { Zz }
      -- Relation symbols
      |  EQ          { Eq }
      |  NEQ         { Neq }
      |  LT          { Lt }
      |  GT          { Gt }
      |  LE          { Le }
      |  GE          { Ge }
      |  PREC        { Prec }
      |  SUCC        { Succ }
      |  IN          { In }
      |  NOTIN       { Notin }
      |  SUBSET      { Subset }
      |  SUPSET      { Supset }
      |  SUBSETE     { Subsete }
      |  SUPSETE     { Supsete }
      |  MOD         { Mod }
      |  CONGR       { Congr }
      |  APPROX      { Approx }
      |  PROP        { Prop }
      -- Logical symbols
      |  AND         { And }
      |  OR          { Or }
      |  NOT         { Not }
      |  IMPLIES     { Implies }
      |  IF          { If }
      |  IFF         { Iff }
      |  FORALL      { Forall }
      |  EXISTS      { Exists }
      |  FALSUM      { Falsum }
      |  TAUT        { Taut }
      |  TURNSTILE   { Turnstile }
      |  TTURNSTILE  { Tturnstile }
      -- Arrows
      |  UARR        { Uarr }
      |  DARR        { Darr }
      |  LARR        { Larr }
      |  TO          { To }
      |  MAPSTO      { Mapsto }
      |  HARR        { Harr }
      |  LLARR       { Llarr }
      -- Additionnal tokens
      |  COMMA       { Comma }
      |  DOT         { Dot }
      |  SEMICOLON   { Semicolon }
      |  QUOTE       { Quote }
      |  FACTO       { Facto }

-- Unary Functions {{{1
op1 :: { UnaryOp }
op1 :  SQRT      { Usqrt }
    |  TEXT      { Utext }
    |  BB        { Ubb }
    |  BBB       { Ubbb }
    |  UCC       { Ucc }
    |  TT        { Utt }
    |  FR        { Ufr }
    |  SF        { Usf }
    |  TILDE     { Utilde }
    |  HAT       { Uhat }
    |  BAR       { Ubar }
    |  UL        { Uul }
    |  VEC       { Uvec }
    |  DOTOP     { Udot }
    |  DDOT      { Uddot }

-- Binary Functions {{{1
op2 :: { BinaryOp }
op2 :  FRAC      { BFrac }
    |  ROOT      { BRoot }
    |  STACKREL  { BStackRel }

-- Delimiters {{{1
lDel :: { LBracket }
lDel :  LDEL     { let (LDEL s, _) = $1 in ldel s }
rDel :: { RBracket }
rDel :  RDEL     { let (RDEL s, _) = $1 in rdel s }

-- Function Definitions {{{1
{

thenE :: Either AsciimathException a -> (a -> Either AsciimathException b) -> Either AsciimathException b
thenE (Left err) _ = Left err
thenE (Right x) f = f x

happyError tokens =
  let (tok, pos) = head tokens in
  Left $ LexicalError (show tok) pos

potentialSpace :: Expr -> [Expr] -> [Expr]
potentialSpace (Simple (SEConst (Diff _))) = (Simple (SEConst SmallSpace) :)
potentialSpace _ = id

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
