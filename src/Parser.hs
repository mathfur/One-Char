{-# LANGUAGE OverloadedStrings, TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}

module Parser
  ( mainParser
  ) where

import Text.Peggy
import Data.Maybe
import Data.List
import Control.Monad.State
import Control.Applicative

import OneCharType
import Expand

[peggy|
expr :: Expr_ = target operation+ { Sequence_ $1 $2 }

target :: Target = '@' { Target '9' "@" }
  / [a-z]+ [0-9] { Target $2 $1 }
operation :: Operation = prefix ('(' inner_parenthsis ')')? { Operation $1 (fromMaybe [] $2) }

inner_parenthsis :: [Expr_] = head_args ('>' tail_args)? { $1 ++ fromMaybe [] $2 }

head_args :: [Expr_] = target* { map (\t -> Sequence_ t []) $1 }
tail_args :: [Expr_] = second_args third_args func? { $1 ++ $2 ++ maybeToList $3 }

second_args :: [Expr_] = expr*
third_args :: [Expr_] = (' ' key ':' expr)* { if (null $1) then [] else [Obj $1] }
func :: Expr_ = ' ' func_args '-' expr { Func $1 $2 }
              / '.' expr           { Func [] $1 }

key :: Key = [a-z]+ [0-9] { Key $2 $1 }
func_args :: [Arg] = func_arg*
func_arg :: Arg = [a-z]+ [0-9] { Arg $2 $1 }
prefix :: Prefix = [a-z]+ [0-9] { Prefix $2 $1 }
|]

mainParser :: String -> Either ParseError Expr_
mainParser = parseString expr ""

main = do
  case (mainParser "@ec4") of
    Right e -> (expandFromExpr e)>>=print
    Left e -> print e
