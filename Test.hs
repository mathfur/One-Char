{-# Language TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}

import Text.Peggy
import Data.Maybe
import Data.List
import Control.Monad.State
import Data.Functor.Identity
import Control.Applicative

data Expr_ = Obj [(Char, Expr_)]
          | Func [Arg] Expr_
          | Sequence_ Target [Operation]
          deriving Show

data Operation = Operation Prefix [Expr_] deriving Show

data Arg = Arg { arg2Char :: Char, arg2String :: String } deriving Show

data Target = Target Char String deriving Show

data Prefix = Prefix Char String deriving Show

-- TODO:
-- * kvsなど対応
-- * a"isA"(b;c;)の展開
-- * e(v-ve)の展開
--  * -> each((v) -> v.e())
-- * e(.e)の展開
[peggy|
expr :: Expr_ = target operation+ { Sequence_ $1 $2 }

target :: Target = [a-z]+ [0-9] { Target $2 $1 }
operation :: Operation = prefix ('(' inner_parenthsis ')')? { Operation $1 (fromMaybe [] $2) }

inner_parenthsis :: [Expr_] = head_args ('>' tail_args)? { $1 ++ fromMaybe [] $2 }

head_args :: [Expr_] = target* { map (\t -> Sequence_ t []) $1 }
tail_args :: [Expr_] = second_args third_args func? { $1 ++ $2 ++ maybeToList $3 }

second_args :: [Expr_] = expr*
third_args :: [Expr_] = (' ' key ':' expr)* { if (null $1) then [] else [Obj $1] }
func :: Expr_ = ' ' func_args '-' expr { Func $1 $2 }
              / '.' expr           { Func [] $1 }

key :: Char = [a-z]
func_args :: [Arg] = func_arg*
func_arg :: Arg = [a-z]+ [0-9] { Arg $2 $1 }
prefix :: Prefix = [a-z]+ [0-9] { Prefix $2 $1 }
|]

expand :: Expr_ -> IO String
expand ( Obj pairs ) = do
  xss <-  mapM expandPair pairs
  return $ "{" ++ (intercalate ", " xss) ++ "}"
    where
      expandPair :: (Char, Expr_) -> IO String
      expandPair (c,expr) = do
        (\a b -> a ++ ": " ++ b) <$> (getWord [c]) <*> (expand expr)
expand (Func as expr) = do
  (\a b -> "function(" ++ a ++ "){ return (" ++ b ++ ") }") <$> (joinToString (intercalate ", ") expandArg as) <*> (expand expr)
    where
      expandArg :: Arg -> IO String
      expandArg = getWord.arg2String
expand (Sequence_ (Target d cs) os) = do
  c_ <- getWord cs
  os_ <- joinToString (intercalate ".") expandOper os
  return $ c_ ++ (if (null os_) then "" else ("." ++ os_))
    where
      expandOper :: Operation -> IO String
      expandOper (Operation (Prefix d cs) es) = do
        (++) <$> (getWord cs) <*> (joinToString (\xs -> "(" ++ (intercalate ", " xs) ++ ")") expand es)

getWord :: String -> IO String
getWord xs = do
  let env = [('e', "each"), ('m', "map"), ('s', "select"), ('f', "function"), ('h', "hash"), ('a', "array"), ('o', "object"),('d', "disable")]
  return $ fromMaybe xs $ lookup (head xs) env

joinToString :: ([String] -> String) -> (a -> IO String) -> [a] -> IO String
joinToString g f as = (mapM f as)>>=(return.g)

main = do
  let samples = [ ("expr", ["a0d1", "a0e1(f0)", "a0f1(a0>a0d1)", "a0e1(> v0-v0e1)", "a0e1(a0>a0e1 k:a0e1 l:a0e1)", "a0e1(a0>k:a0e1 l:a0e1 v0w2-v0e1)"]), ("head_args", ["a0b1"]), ("second_args", [" a0e1 a0e1"]), ("third_args", [" k:a0e1", " l:a0e1", " k:a0e1 l:a0e1"]), ("func", [" v0-v0e1", " v1w2-v0e1"]), ("inner_parenthsis", ["a0e1", "a0e1 a0e1", "a0e1 k:a0e1", "a0e1 k:a0e1 l:a0e1", "a0e1 k:a0e1 v1-v0e1"])]
  forM_ (fromJust $ lookup "expr" samples) (\sample ->
    case (parseString expr "" sample) of
      Right e -> do
        result <- mapM expand [e]
        putStrLn ( sample ++ "\t=>\t" ++ (intercalate "| " result) )
      Left e -> do
        print e
    )
