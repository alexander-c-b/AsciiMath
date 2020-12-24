{-# LANGUAGE LambdaCase #-}
module Passes (passes,needsSpace,prependSpace) where
import Data.Bool (bool)
import Data.List.Split (splitWhen)
import Ast (Constant(..),Code,Expr(..),SimpleExpr(..),walkC)

passes :: Code -> Code
passes = walkC prependSpace . walkC matrix

matrix :: Code -> Code
matrix es = case map cols $ rows es of
              [[code]] -> code
              x -> [Simple $ Matrix x]
    where rows = splitWhen $ isConstant DoubleSemicolon
          cols = splitWhen $ isConstant Ampersand

isConstant :: Constant -> Expr -> Bool
isConstant c = \case
    Simple (SEConst x) | c == x -> True
    _ -> False

prependSpace :: Code -> Code
prependSpace [] = []
prependSpace (x:xs) = x : concatMap go xs
    where go :: Expr -> [Expr]
          go e = bool (<> [Simple (SEConst Space)]) id (needsSpace e) [e]

needsSpace :: Expr -> Bool
needsSpace = \case
    -- Differentials; e.g. dx, dA
    (Simple (SEConst (Letters ('d':_:[])))) -> True
    _ -> False
