module Hutton where

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y

printExpr :: Expr -> String
printExpr (Lit a)   = show a
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b
