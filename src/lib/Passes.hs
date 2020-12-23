{-# LANGUAGE LambdaCase #-}
module Passes (matrix) where
import Data.List.Split (splitWhen)
import Ast (Constant(Ampersand,DoubleSemicolon)
           ,Code,Expr(Simple),SimpleExpr(SEConst,Matrix),walkC)

matrix :: Code -> Code
matrix = walkC $ \es -> case map cols $ rows es of
                          [[code]] -> code
                          x -> [Simple $ Matrix x]
    where rows = splitWhen $ isConstant DoubleSemicolon
          cols = splitWhen $ isConstant Ampersand

isConstant :: Constant -> Expr -> Bool
isConstant c = \case
    Simple (SEConst x) | c == x -> True
    _ -> False
