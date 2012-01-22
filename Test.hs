{-# Language TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}

import Text.Peggy
import Data.Maybe

data Word = Word String deriving Show
data Term = Term Word [Term] deriving Show

[peggy|
nums :: [Int]
  = num*
num ::: Int
  = [0-9]+ { read $1 }
word ::: Word
  = [a-z] { Word [$1] }
term :: Term
  = word ("(" term* ")")? { Term $1 $ fromMaybe [] $2 }
|]

main :: IO ()
main = do
  print $ parseString word "" "a"
  print $ parseString term "" "a"
  print $ parseString term "" "a()"
  print $ parseString term "" "a(b)"
  print $ parseString term "" "a(bc)"
