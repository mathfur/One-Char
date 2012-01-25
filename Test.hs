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

--term2string :: Term -> String
--term2string (Term c ts) =  (Foo c) ++ (if (length ts == 0) then "" else ("(" ++ (concat $ map term2string ts) ++ ")"))

--main :: IO ()
--main = do
--  case (parseString term "" "a") of Right e -> print $ term2string e; Left e -> print e
--  case (parseString term "" "a(b;)") of Right e -> print $ term2string e; Left e -> print e
--  case (parseString term "" "a(b;c;)") of Right e -> print $ term2string e; Left e -> print e


--  pairs <- get
--  pairs :: [(Char, String)]
--  lookup 'a' pairs :: Maybe String

saveSource :: Char -> String -> StateT [(Char, String)] Identity ()
saveSource c source = do 
  pairs <- get
  put $ pairs ++ [(c, source)]
  return ()
  --return $ lookup c pairs

expandChar :: Char -> StateT [(Char, String)] Identity (Maybe String)
expandChar c = do
  pairs <- get
  return $ lookup c pairs

g :: String -> [String] -> String
g s x = s ++ "(" ++ (intercalate ";" x) ++ ")"

--Term (Word 'a') [Term (Word 'b') [], Term (Word 'c') []]
--に[('a', "art"), ('b', "braid"), ('c', "chain")]を入れると
--art(braid, chain)が出力されるようにしたい


f :: Term -> StateT [(Char, String)] Identity String
f (Term (Word c) ts) = do
  pairs <- get
  let source = fromMaybe [c] $ lookup c pairs -- :: String
  xs <- (mapM f ts) -- :: m [String] => xs :: [String]
  return $ g source xs
-- mapM_ :: (Term -> StateT [(Char, String)] Identity String) -> [Term] -> StateT [(Char, String)] Identity [String]
-- mapM_ f ts :: StateT [(Char, String)] Idenfity [String]
--
-- m String

--main = print $ evalState stream []
main = do
  let environment = [('f', "function"), ('h', "hash"), ('a', "array"), ('o', "object")] 
  let term2 = parseString term "" "a(h;o;)"
  case term2 of
    Right term3 -> print $ evalState (f $ term3) environment
    Left e -> print e
