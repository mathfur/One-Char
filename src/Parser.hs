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
expr :: Expr_ = '`' inner_backquote '`' { PlainText $1 }
  / target operation+ { Sequence_ $1 $2 }

inner_backquote :: String = [^`]+ ('\`' [^`]*)* { $1 ++ (intercalate [] $ map (\cs -> ['`'] ++ cs) $2) }

target :: Target = [_@] { Target '9' [$1] }
  / [a-z]+ [0-9] { Target $2 $1 }
operation :: Operation = prefix ('(' inner_parenthsis ')')? { Operation $1 (fromMaybe [] $2) }

inner_parenthsis :: [Expr_] = head_args ('>' tail_args)? { $1 ++ fromMaybe [] $2 }

head_args :: [Expr_] = target* { map (\t -> Sequence_ t []) $1 }
tail_args :: [Expr_] = second_args third_args func? { $1 ++ $2 ++ maybeToList $3 }

second_args :: [Expr_] = expr*
third_args :: [Expr_] = (' ' key ':' expr)* { if (null $1) then [] else [Obj $1] }
func :: Expr_ = ' ' func_args '-' expr (';' expr)* { Func $1 ($2:$3) }
              / '.' expr (';' expr)*  { Func [] ($1:$2) }

key :: Key = [a-zA-Z_]+ [0-9] { Key $2 $1 }
func_args :: [Arg] = func_arg*
func_arg :: Arg = [a-zA-Z_]+ [0-9] { Arg $2 $1 }
prefix :: Prefix = '$' { Prefix '9' "$" }
  / [a-zA-Z_]+ [0-9] { Prefix $2 $1 }
|]

mainParser :: String -> Either ParseError Expr_
mainParser = parseString expr ""

main = do
  let x1 = "@ec4(>`each.function(e){ return 10 }`)"
  let x2 = "_D3$(>`\"#{BASE_DIR}/foo/*.rb\"`)"
  let x3 = "@e4(> s3-s3e4)"
  let f1 = "s3"
  let o1 = "s3"
  let func_args1 = "v3w4"
  case (mainParser x2) of
    Right e -> do
      (expandFromExpr e)>>=putStrLn
      putStrLn "-- パース結果 --"
      print e
    Left e -> print e
