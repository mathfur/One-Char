{-# Language TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}

import Text.Peggy
import Data.Maybe
import Data.List
import Control.Monad.State
import Data.Functor.Identity

data Word = Word Char deriving Show
data Term = Term Word [Term] deriving Show

[peggy|
term ::: Term = word ('(' terms ')')? { Term (Word $1) (fromMaybe [] $2) }  -- a(b;cd();)
terms ::: [Term] = (term ';')+
word :: Char = [a-z]
|]

toString :: String -> [String] -> String
toString name args = name ++ "(" ++ (intercalate ";" args) ++ ")"

expandTerm :: Term -> StateT [(Char, String)] Identity String
expandTerm (Term (Word c) ts) = do
  source <- gets ((fromMaybe [c] ).(lookup c))
  xs <- (mapM expandTerm ts)
  return $ toString source xs

main = do
  let environment = [('f', "function"), ('h', "hash"), ('a', "array"), ('o', "object")] 
  case (parseString term "" "a(h;o;)") of
    Right t -> print $ evalState (expandTerm t) environment
    Left e -> print e
