{-# LANGUAGE TemplateHaskell #-}

module OneCharType where

data Expr_ = Obj [(Key, Expr_)]
          | Func [Arg] [Expr_]
          | Sequence_ Target [Operation]
          | PlainText String
          deriving Show

data Operation = Operation Prefix [Expr_] deriving Show

-- Charには1桁の数字が入る
data Arg = Arg { arg2Char :: Char, arg2String :: String } deriving Show

-- Charには1桁の数字が入る
data Target = Target Char String deriving Show
data Prefix = Prefix Char String deriving Show
data Key = Key Char String deriving Show
