{-# Language TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}

import Text.Peggy
import Data.Maybe

data Word = Word String deriving Show
data Term = Term Word [Term] deriving Show

[peggy|
words_ ::: (String, String) = word* '(' [0-9]+ ')' { ($1, $2) }
words2 ::: (String, Maybe String) = word* ([0-9]+)? { ($1, $2) }
word :: Char = [a-z]
|]

main :: IO ()
main = do
  --print $ parseString word "" "a"
  --print $ parseString words_ "" "a"
  print $ parseString words2 "" "ab123"
