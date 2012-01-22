{-# Language TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}

import Text.Peggy
import Data.Maybe

data Word = Word String deriving Show
data Term = Term Char [Term] deriving Show

[peggy|
term ::: (Char, Maybe String) = word ('(' (digit_)* ')')? { ($1, $2) }  -- a(b;cd();)
digit_ :: Char = [0-9]
word :: Char = [a-z]
|]

main :: IO ()
main = do
  --print $ parseString word "" "a"
  --print $ parseString words_ "" "a"
  print $ parseString term "" "a()"
  print $ parseString term "" "a(3)"
  print $ parseString term "" "a(981)"
