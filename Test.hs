{-# Language TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}

import Text.Peggy
import Data.Maybe
import Data.List
import Control.Monad.State
import Data.Functor.Identity

data Prefix = Prefix String deriving Show
data Term = Term Prefix [Term] deriving Show

-- TODO: 
-- * なぜtermsが途中までのヒットでもマッチしてしまうか調べる
-- * a"isA"(b;c;)の展開
-- * e(.e)の展開
[peggy|
term :: Term = prefix ('(' terms ')')? { Term $1 (fromMaybe [] $2) }  -- a(b;cd();)
terms :: [Term] = term+
                / (term ';')+
prefix :: Prefix = '\"' abbr_word '\"' { Prefix $1 }
                 / [a-z] { Prefix [$1] }
abbr_word :: String = [a-zA-Z0-9]+
|]

toString :: String -> [String] -> String
toString name args = name ++ (if (length args == 0) then "" else ("(" ++ (intercalate "," args) ++ ")"))

expandTerm :: Term -> StateT [(Char, String)] Identity String
expandTerm (Term (Prefix cs) ts) = do
  source <- gets ((fromMaybe [head cs] ).(lookup $ head cs))
  xs <- (mapM expandTerm ts)
  return $ toString source xs

main = do
  let environment = [('f', "function"), ('h', "hash"), ('a', "array"), ('o', "object")] 
  let samples =  ["a(h;o;)", "a(ho);", "a;\"isA\";"]
  forM_ samples (\sample ->
    case (parseString terms "" sample) of
      Right ts -> putStrLn ( sample ++ "\t=>\t" ++ (intercalate "\t" (evalState (mapM expandTerm ts) environment )))
      Left e -> print e
    )
