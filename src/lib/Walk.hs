import Control.Applicative (liftA,liftA2,liftA3)
import Language.AST.Walk   (Walkable(walk,walkM,query))

-- Instance declarations
instance Walkable Expr Expr where
    walkM f x = walkExprM f x >>= f
    query f e = f e <> queryExpr f e

instance Walkable SimpleExpr SimpleExpr where
    walkM f x = walkSimpleExprM f x >>= f
    query f s = f s <> querySimpleExpr f s

instance Walkable Expr Expr where
    walkM = walkSimpleExprM
    query = querySimpleExpr

-- Walk functions
walkExprM :: (Walkable a [Expr],Walkable a SimpleExpr
             ,Walkable a RBracket,Walkable a LBracket
             ,Walkable a BinaryOp,Walkable a UnaryOp
             ,Walkable a Constant
             ,Monad m, Applicative m,Functor m)
          => (a -> m a) -> Expr -> Expr
walkExprM f = \case
    Simple s          -> (liftA Simple) (walkM f s)
    Frac s1 s2        -> (liftA2 Frac)  (walkM f s1) (walkM f s2)
    Under s1 s2       -> (liftA2 Under) (walkM f s1) (walkM f s2)
    Super s1 s2       -> (liftA2 Super) (walkM f s1) (walkM f s2)
    SubSuper s1 s2 s3 ->
        (liftA3 SubSuper) (walkM f s1) (walkM f s2) (walkM f s3)

walkSimpleExprM :: (Walkable a [Expr],Walkable a SimpleExpr
                   ,Walkable a RBracket,Walkable a LBracket
                   ,Walkable a BinaryOp,Walkable a UnaryOp
                   ,Walkable a Constant
                   ,Monad m, Applicative m,Functor m)
                => (a -> m a) -> SimpleExpr -> SimpleExpr
walkSimpleExprM f = \case
    SEConst c     -> (liftA SEConst)   (walkM f c)
    Delimited lbr es rbr ->
        (liftA3 Delimited) (walkM f lbr) (walkM f es) (walkM f rbr)
    Matrix cs     -> (liftA Matrix)    (walkM f cs)
    UnaryApp op s -> (liftA2 UnaryApp) (walkM f op) (walkM f s)
    BinaryApp op s1 s2 ->
        (liftA3 BinaryApp) (walkM f op) (walkM f s1) (walkM f s2)

-- Query functions
queryExpr :: (Walkable a [Expr],Walkable a SimpleExpr
             ,Walkable a RBracket,Walkable a LBracket
             ,Walkable a BinaryOp,Walkable a UnaryOp
             ,Walkable a Constant,Monoid c)
          => (a -> c) -> Expr -> c
queryExpr f = \case
    Simple s          -> query f s
    Frac s1 s2        -> query f s1 <> query f s2
    Under s1 s2       -> query f s1 <> query f s2
    Super s1 s2       -> query f s1 <> query f s2
    SubSuper s1 s2 s3 -> query f s1 <> query f s2 <> query f s3

querySimpleExpr :: (Walkable a [Expr],Walkable a SimpleExpr
                   ,Walkable a RBracket,Walkable a LBracket
                   ,Walkable a BinaryOp,Walkable a UnaryOp
                   ,Walkable a Constant,Monoid c)
                => (a -> c) -> SimpleExpr -> c
querySimpleExpr f = \case
    SEConst c            -> query f c
    Delimited lbr es rbr -> query f lbr <> query f es <> query f rbr
    Matrix cs            -> query f cs
    UnaryApp op s        -> query f op  <> query f s
    BinaryApp op s1 s2   -> query f op  <> query f s1 <> query f s2
    Raw string           -> query f string
