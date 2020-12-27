{-# LANGUAGE LambdaCase
  , MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances #-}
module Ast where

-- Constants : variables, numbers, etc.
data Constant =
  Letters String
  | Number String
  | GreekLetter String
  | StdFun String
  | Diff String
  -- Operation symbols
  | Add | Sub | Mul | Mmul | Mmmul | Sslash | Bbslash
  | Times | Div | Comp | Oplus | Otimes | Odot
  | Sum | Prod | Wedge | Wwedge | Vv | Vvv | Nn | Nnn | Uu | Uuu
  -- Miscellaneous symbols
  | Inte | Oint | Del | Grad | Addsub | Void | Infty | Aleph
  | Angle | Therefore | Abs | Cdots | Vdots | Ddots | Bslash
  | Quad | Diamond | Square | Lfloor | Rfloor | Lceil | Rceil
  | Cc | Ensnn | Qq | Rr | Zz | Space
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
  -- Additionnal symbols
  | Comma | Dot | Semicolon | Quote | Facto
  deriving (Show, Eq)

-- Unary operators
data UnaryOp =
  Usqrt | Utext
  | Ubb | Ubbb | Ucc | Utt | Ufr | Usf
  | Utilde | Uhat | Ubar | Uul | Uvec | Udot | Uddot
  deriving (Show, Eq)

-- Binary operators
data BinaryOp = BFrac | BRoot | BStackRel deriving (Show, Eq)

-- Left brackets
data LBracket = LPar | LCro | LBra | LChe | LBraCons deriving (Show, Eq)

-- Right brackets
data RBracket = RPar | RCro | RBra | RChe | RBraCons deriving (Show, Eq)

-- Simple expressions
data SimpleExpr =
  SEConst Constant
  | Delimited LBracket Code RBracket
  | UnaryApp UnaryOp SimpleExpr
  | BinaryApp BinaryOp SimpleExpr SimpleExpr
  | Raw String  -- raw text, redered in a \textrm
  | WithSpace SimpleExpr
  deriving(Show, Eq)

-- Global expressions
data Expr =
  Simple SimpleExpr
  | Frac SimpleExpr SimpleExpr
  | Under SimpleExpr SimpleExpr
  | Super SimpleExpr SimpleExpr
  | SubSuper SimpleExpr SimpleExpr SimpleExpr
  deriving (Show, Eq)

-- Whole asciimath code
data Code = Matrix [[[Expr]]] | Exprs [Expr] deriving (Show,Eq)
