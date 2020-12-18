{-# LANGUAGE LambdaCase, MultiParamTypeClasses, FlexibleContexts #-}
module Ast (Code,Expr(..),SimpleExpr(..),RBracket(..),LBracket(..)
           ,BinaryOp(..),UnaryOp(..),Constant(..)) where
import Control.Applicative (liftA,liftA2,liftA3)
import Language.AST.Walk   (Walkable(..))

-- Constants : variables, numbers, etc.
data Constant =
  Letter Char
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

-- Instance declarations
instance Walkable Expr Expr where
    walkM f x = walkExprM f x >>= f
    query f e = f e <> queryExpr f e

instance Walkable SimpleExpr SimpleExpr where
    walkM f x = walkSimpleExprM f x >>= f
    query f s = f s <> querySimpleExpr f s

instance Walkable SimpleExpr Expr where
    walkM = walkExprM
    query = queryExpr

instance Walkable Expr SimpleExpr where
    walkM = walkSimpleExprM
    query = querySimpleExpr

-- Walk functions
walkExprM       :: (Walkable a Expr,Walkable a SimpleExpr
                   ,Monad m,Applicative m,Functor m)
                => (a -> m a) -> Expr -> m Expr
walkSimpleExprM :: (Walkable a Expr,Walkable a SimpleExpr
                   ,Monad m,Applicative m,Functor m)
                => (a -> m a) -> SimpleExpr -> m SimpleExpr

walkExprM f = \case
    Simple s          -> (liftA Simple) (walkM f s)
    Frac s1 s2        -> (liftA2 Frac)  (walkM f s1) (walkM f s2)
    Under s1 s2       -> (liftA2 Under) (walkM f s1) (walkM f s2)
    Super s1 s2       -> (liftA2 Super) (walkM f s1) (walkM f s2)
    SubSuper s1 s2 s3 ->
        (liftA3 SubSuper) (walkM f s1) (walkM f s2) (walkM f s3)

walkSimpleExprM f = \case
    SEConst c     -> return $ SEConst c
    Delimited lbr es rbr ->
        (liftA3 Delimited) (return lbr) (walkM f es) (return rbr)
    Matrix cs     -> (liftA Matrix)    (walkM f cs)
    UnaryApp op s -> (liftA2 UnaryApp) (return op) (walkM f s)
    BinaryApp op s1 s2 ->
        (liftA3 BinaryApp) (return op) (walkM f s1) (walkM f s2)
    Raw string   -> return $ Raw string

-- Query functions
queryExpr       :: (Walkable a Expr,Walkable a SimpleExpr,Monoid c)
                => (a -> c) -> Expr -> c
querySimpleExpr :: (Walkable a Expr,Walkable a SimpleExpr,Monoid c)

                => (a -> c) -> SimpleExpr -> c

queryExpr f = \case
    Simple s          -> query f s
    Frac s1 s2        -> query f s1 <> query f s2
    Under s1 s2       -> query f s1 <> query f s2
    Super s1 s2       -> query f s1 <> query f s2
    SubSuper s1 s2 s3 -> query f s1 <> query f s2 <> query f s3

querySimpleExpr f = \case
    SEConst _            -> mempty
    Delimited _ es _ -> query f es
    Matrix cs            -> query f cs
    UnaryApp _ s        -> query f s
    BinaryApp _ s1 s2   -> query f s1 <> query f s2
    Raw _           -> mempty
