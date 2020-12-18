{-# LANGUAGE LambdaCase #-}
module Walk () where
import Control.Applicative (liftA,liftA2,liftA3)
import Language.AST.Walk   (Walkable)

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
type Walker b = (Walkable a Expr,Walkable a SimpleExpr
                ,Monad m,Applicative m,Functor m)
             => (a -> m a) -> b -> m b
walkExprM       :: Walker Expr
walkSimpleExprM :: Walker SimpleExpr

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
        (liftA3 Delimited) (return lbr) (walkM f es) (return lbr)
    Matrix cs     -> (liftA Matrix)    (walkM f cs)
    UnaryApp op s -> (liftA2 UnaryApp) (return op) (walkM f s)
    BinaryApp op s1 s2 ->
        (liftA3 BinaryApp) (return op) (walkM f s1) (walkM f s2)
    Raw string   -> return $ Raw string

-- Query functions
type Querant b = (Walkable a [Expr],Walkable a SimpleExpr,Monoid c)
              => (a -> c) -> b -> c
queryExpr       :: Querant Expr
querySimpleExpr :: Querant SimpleExpr

queryExpr f = \case
    Simple s          -> query f s
    Frac s1 s2        -> query f s1 <> query f s2
    Under s1 s2       -> query f s1 <> query f s2
    Super s1 s2       -> query f s1 <> query f s2
    SubSuper s1 s2 s3 -> query f s1 <> query f s2 <> query f s3

querySimpleExpr f = \case
    SEConst c            -> mempty
    Delimited lbr es rbr -> query f es
    Matrix cs            -> query f cs
    UnaryApp op s        -> query f s
    BinaryApp op s1 s2   -> query f s1 <> query f s2
    Raw string           -> mempty
