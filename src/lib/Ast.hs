{-# LANGUAGE LambdaCase #-}
module Ast where

data Code = Matrix [[[Expr]]] | Exprs [Expr] 
  deriving (Show,Eq)
data Expr = Simple Simple | Frac Simple Simple
  deriving (Show,Eq)
data Simple = Term Term | Unary UnaryOp Term | Binary BinaryOp Term Term
  deriving (Show,Eq)
data Term = STerm STerm | Under STerm STerm | Super STerm STerm
          | SubSuper STerm STerm STerm
          deriving (Show,Eq)
data STerm = Text String | Delimited Delimiter Code Delimiter 
           | Constant Constant
           deriving (Show,Eq)

data Constant
  = Letters String
  | Number String
  | GreekLetter String
  | Diff String
  -- Operation symbols
  | Add | Sub | Mul | Mmul | Mmmul | Sslash | Bbslash
  | Times | Div | Comp | Oplus | Otimes | Odot
  | Sum | Prod | Wedge | Wwedge | Vv | Vvv | Nn | Nnn | Uu | Uuu
  -- Miscellaneous symbols
  | Inte | Oint | Del | Grad | Addsub | Void | Infty | Aleph
  | Angle | Therefore | Abs | Cdots | Vdots | Ddots | Bslash
  | Quad | Diamond | Square | Lfloor | Rfloor | Lceil | Rceil
  | Cc | Ensnn | Qq | Rr | Zz | Space | SmallSpace
  -- Relation symbols
  | Eq | Neq | Lt | Gt | Le | Ge | Prec | Succ
  | In | Notin | Subset | Supset | Subsete | Supsete
  | Mod | Congr | Approx | Prop
  -- Logical symbols
  | And | Or | Not | Implies | If | Iff | Forall | Exists
  | Falsum | Taut | Turnstile | Tturnstile
  -- Arrows
  | Uarr | Darr | Larr | To
  | Mapsto | Harr | Llarr
  -- Additional symbols
  | Comma | Dot | Semicolon | Quote | Facto
  deriving (Show, Eq)

data UnaryOp
  = Usqrt | Utext
  | Ubb | Ubbb | Ucc | Utt | Ufr | Usf
  | Utilde | Uhat | Ubar | Uul | Uvec | Udot | Uddot
  | SimpleUnary String
  deriving (Show, Eq)

data BinaryOp = BFrac | BRoot | BStackRel deriving (Show, Eq)

data Delimiter = Parenthesis | Bracket | Brace | AngleBracket | Invisible
  deriving (Show,Eq)
