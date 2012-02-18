module Helpers where

import OneCharType

getNonFuncAndFunc :: [Expr_] -> ([Expr_], Maybe Expr_)
getNonFuncAndFunc [] = ([], Nothing)
getNonFuncAndFunc as = if (isFunc las) then (init as, Just las) else (as, Nothing)
  where
    las = last as
