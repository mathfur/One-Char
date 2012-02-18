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
expr :: Expr_ = '\"' inner_doublequote '\"' { PlainText $ "\"" ++ $1 ++ "\""}
  / '`' inner_backquote '`' { PlainText $1 }
  / target operation+ { Sequence_ $1 $2 }

inner_doublequote :: String = [^"]+ ('\\\"' [^"]*)* { $1 ++ (intercalate [] $ map (\cs -> ['\"'] ++ cs) $2) }
inner_backquote :: String = [^`]+ ('\`' [^`]+)* { $1 ++ (intercalate [] $ map (\cs -> ['`'] ++ cs) $2) }

target :: Target = [#@] { Target '9' [$1] }
  / [a-zA-Z]+ [0-9] { Target $2 $1 }
operation :: Operation = one_char_prefix '(' inner_parenthsis ')' { Operation $1 $2 }
  / prefix ('(' inner_parenthsis ')')? { Operation $1 (fromMaybe [] $2) }

inner_parenthsis :: [Expr_] = head_args ('>' tail_args)? { $1 ++ fromMaybe [] $2 }

head_args :: [Expr_] = target* { map (\t -> Sequence_ t []) $1 }
tail_args :: [Expr_] = second_args third_args func? { $1 ++ $2 ++ maybeToList $3 }

second_args :: [Expr_] = (' ' expr)*
third_args :: [Expr_] = (' ' key ':' expr)* { if (null $1) then [] else [Obj $1] }
func :: Expr_ = ' ' func_args '-' expr (';' expr)* { Func $1 ($2:$3) }
              / '.' expr (';' expr)*  { Func [] ($1:$2) }

key :: Key = [a-zA-Z]+ [0-9] { Key $2 $1 }
func_args :: [Arg] = func_arg*
func_arg :: Arg = [a-zA-Z]+ [0-9] { Arg $2 $1 }
prefix :: Prefix = [\$=] { Prefix '9' [$1] }
  / [a-zA-Z]+ [0-9] { Prefix $2 $1 }
one_char_prefix :: Prefix = [esmt] { Prefix '9' [$1] }
|]

mainParser :: String -> Either ParseError Expr_
mainParser = parseString expr ""

main = do
  let x1 = "@ec4(>`each.function(e){ return 10 }`)"
  let x2 = "#D3$(>`\"#{BASE_DIR}/foo/*.rb\"`)"
  let x3 = "@e4(> s3-s3e4)"
  let x4 = "#D3"
  let x5 = "@g1"
  let x6 = "@e4(> s3-s3e4;s3e4)"
  let x7 = "#F4r4(>`\"foo.txt\"`)"
  let x8 = "\"foo.txt\""
  let x9 = "#F4r4(>\"foo.txt\")"
  let x10 = "#F4r4(>\"foo.txt\")=(cnt0)"
  let x11 = "#f1(>`10`)=(x1y1)"
  let x12 = "e4e(e4)"
  let x13 = "e4e4(> e4-e4e4)"
  let x14 = "F4r4"
  let x15 = "ar3e4(> #p4 p5-p5d7)"
  let f1 = "s3"
  let o1 = "s3"
  let func_args1 = "v3w4"
  case (mainParser x15) of
    Right e -> do
      (expandFromExpr e)>>=putStrLn
      putStrLn "-- パース結果 --"
      print e
    Left e -> print e
