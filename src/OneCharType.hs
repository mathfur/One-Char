{-# LANGUAGE TemplateHaskell #-}

module OneCharType where

data Expr_ = Obj [(Char, Expr_)]
          | Func [Arg] Expr_
          | Sequence_ Target [Operation]
          deriving Show

data Operation = Operation Prefix [Expr_] deriving Show

-- Charには1桁の数字が入る
data Arg = Arg { arg2Char :: Char, arg2String :: String } deriving Show

-- Charには1桁の数字が入る
data Target = Target Char String deriving Show

-- Charには1桁の数字が入る
data Prefix = Prefix Char String deriving Show
