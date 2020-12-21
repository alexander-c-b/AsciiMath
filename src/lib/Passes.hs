{-# LANGUAGE LambdaCase #-}
module Passes (matrix,isConstant) where
import Language.AST.Walk (Walkable(walk))
import Data.List.Split   (splitWhen)
import Ast

matrix :: Code -> Code
matrix = walk $ \es -> case map cols $ rows es of
                         [[code]] -> code
                         x -> [Simple $ Matrix x]
    where rows = splitWhen $ isConstant DoubleSemicolon
          cols = splitWhen $ isConstant Ampersand

isConstant :: Constant -> Expr -> Bool
isConstant c = \case
    Simple (SEConst x) | c == x -> True
    _ -> False
