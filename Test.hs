{-# Language TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}

import Text.Peggy
import Data.Maybe
import Data.List
import Control.Monad.State
import Data.Functor.Identity

data Word = Word Char deriving Show
data Term = Term Word [Term] deriving Show

[peggy|
term ::: Term = word ('(' (term ';')+ ')')? { Term (Word $1) (fromMaybe [] $2) }  -- a(b;cd();)
word :: Char = [a-z]
|]

g :: String -> [String] -> String
g s x = s ++ "(" ++ (intercalate ";" x) ++ ")"

expandTerm :: Term -> StateT [(Char, String)] Identity String
expandTerm (Term (Word c) ts) = do
  pairs <- get
  let source = fromMaybe [c] $ lookup c pairs
  xs <- (mapM expandTerm ts)
  return $ g source xs

main = do
  let environment = [('f', "function"), ('h', "hash"), ('a', "array"), ('o', "object")] 
  case (parseString term "" "a(h;o;)") of
    Right t -> print $ evalState (expandTerm t) environment
    Left e -> print e
