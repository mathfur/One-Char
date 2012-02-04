{-# LANGUAGE TemplateHaskell #-}

module OneCharType where

data Expr_ = Obj [(Char, Expr_)]
          | Func [Arg] Expr_
          | Sequence_ Target [Operation]
          deriving Show

data Operation = Operation Prefix [Expr_] deriving Show

data Arg = Arg { arg2Char :: Char, arg2String :: String } deriving Show

data Target = Target Char String deriving Show

data Prefix = Prefix Char String deriving Show
