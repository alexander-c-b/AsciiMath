{-# LANGUAGE LambdaCase
  , MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances #-}
module Ast (Code,Expr(..),SimpleExpr(..),RBracket(..),LBracket(..)
           ,BinaryOp(..),UnaryOp(..),Constant(..),walkC,walkS) where

-- Constants : variables, numbers, etc.
data Constant =
  Letters String
  | Number String
  | GreekLetter String
  | StdFun String
  -- Operation symbols
  | Add | Sub | Mul | Mmul | Mmmul | Sslash | Bbslash
  | Times | Div | Comp | Oplus | Otimes | Odot
  | Sum | Prod | Wedge | Wwedge | Vv | Vvv | Nn | Nnn | Uu | Uuu
  -- Miscellaneous symbols
  | Inte | Oint | Del | Grad | Addsub | Void | Infty | Aleph
  | Angle | Therefore | Abs | Cdots | Vdots | Ddots | Bslash
  | Quad | Diamond | Square | Lfloor | Rfloor | Lceil | Rceil
  | Cc | Ensnn | Qq | Rr | Zz | Space
  -- Matrix symbols
  | Ampersand | DoubleSemicolon
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
  | Unit | Num
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
  | Matrix [[Code]]
  | UnaryApp UnaryOp SimpleExpr
  | BinaryApp BinaryOp SimpleExpr SimpleExpr
  | Raw String  -- raw text, redered in a \textrm
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
type Code = [Expr]

-----------------------
-- AST Walking
-----------------------
walkC :: (Code -> Code) -> Code -> Code
walkC f = f . walkCodeSimple (walkSimpleCode f)

walkS :: (SimpleExpr -> SimpleExpr) -> SimpleExpr -> SimpleExpr
walkS f = f . walkSimpleCode (walkCodeSimple f)

walkCodeSimple :: (SimpleExpr -> SimpleExpr) -> Code -> Code
walkCodeSimple f = map $ \case
    Simple s          -> Simple   (f s)
    Frac s1 s2        -> Frac     (f s1) (f s2)
    Under s1 s2       -> Under    (f s1) (f s2)
    Super s1 s2       -> Super    (f s1) (f s2)
    SubSuper s1 s2 s3 -> SubSuper (f s1) (f s2) (f s3)

walkSimpleCode :: (Code -> Code) -> SimpleExpr -> SimpleExpr
walkSimpleCode f = let walkF = walkSimpleCode f in \case
    SEConst c            -> SEConst c
    Delimited lbr es rbr -> Delimited lbr (f es) rbr
    Matrix cs            -> Matrix (map (map f) cs)
    UnaryApp op s        -> UnaryApp op (walkF s)
    BinaryApp op s1 s2   -> BinaryApp op (walkF s1) (walkF s2)
    Raw string           -> Raw string
