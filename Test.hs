{-# Language TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}

import Text.Peggy
import Data.Maybe
import Data.List
import Control.Monad.State
import Data.Functor.Identity

data Word = Word Char deriving Show
data Term = Term Word [Term] deriving Show


saveSource :: Char -> String -> StateT [(Char, String)] Identity ()
saveSource c source = do 
  pairs <- get
  put $ pairs ++ [(c, source)]
  return ()

expandChar :: Char -> StateT [(Char, String)] Identity (Maybe String)
expandChar c = do
  pairs <- get
  return $ lookup c pairs

g :: String -> [String] -> String
g s x = s ++ "(" ++ (intercalate ";" x) ++ ")"

f :: Term -> StateT [(Char, String)] Identity String
f (Term (Word c) ts) = do
  pairs <- get
  let source = fromMaybe [c] $ lookup c pairs
  xs <- (mapM f ts)
  return $ g source xs

[peggy|
term ::: Term = word ('(' (term ';')+ ')')? { Term (Word $1) (fromMaybe [] $2) }  -- a(b;cd();)
word :: Char = [a-z]
|]

main = do
  let environment = [('f', "function"), ('h', "hash"), ('a', "array"), ('o', "object")] 
  case (parseString term "" "a(h;o;)") of
    Right term3 -> print $ evalState (f $ term3) environment
    Left e -> print e
