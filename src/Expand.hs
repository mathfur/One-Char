{-# LANGUAGE OverloadedStrings, TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}

module Expand
  ( expandFromExpr
  ) where

import qualified Data.ByteString.Char8 as B
import           Control.Applicative
import           Data.Maybe
import           Data.List

import OneCharType

expandFromExpr :: Expr_ -> IO String
expandFromExpr ( Obj pairs ) = do
  xss <-  mapM expandPair pairs
  return $ "{" ++ (intercalate ", " xss) ++ "}"
    where
      expandPair :: (Char, Expr_) -> IO String
      expandPair (c,expr) = do
        (\a b -> a ++ ": " ++ b) <$> (getWord [c]) <*> (expandFromExpr expr)
expandFromExpr (Func as expr) = do
  (\a b -> "function(" ++ a ++ "){ return (" ++ b ++ ") }") <$> (joinToString (intercalate ", ") expandArg as) <*> (expandFromExpr expr)
    where
      expandArg :: Arg -> IO String
      expandArg = getWord.arg2String
expandFromExpr (Sequence_ (Target d cs) os) = do
  c_ <- getWord cs
  os_ <- joinToString (intercalate ".") expandOper os
  return $ c_ ++ (if (null os_) then "" else ("." ++ os_))
    where
      expandOper :: Operation -> IO String
      expandOper (Operation (Prefix d cs) es) = do
        (++) <$> (getWord cs) <*> (joinToString (\xs -> "(" ++ (intercalate ", " xs) ++ ")") expandFromExpr es)

getWord :: String -> IO String
getWord xs = do
  dicWords <- (B.readFile "../dic/dic.txt">>=(return.(B.split '\n')))
  return $ B.unpack (candidate dicWords)
    where
      candidate as = if (0 == length (candidates as)) then "" else (head (candidates as))
      candidates as = filter (isIncludedByMeaningOfEachWord $ B.pack xs) as

-- 例) as = 'intzn', bs = 'internationalization' -> true
isIncludedByMeaningOfEachWord :: B.ByteString -> B.ByteString -> Bool
isIncludedByMeaningOfEachWord as bs
  | (B.length as == 0) = True
  | (B.length bs == 0) = False
  | otherwise = (isIncludedByMeaningOfEachWord (B.tail as) bs')
    where
      bs' = snd $ B.breakEnd ((==) $ (B.head as)) bs

joinToString :: ([String] -> String) -> (a -> IO String) -> [a] -> IO String
joinToString g f as = (mapM f as)>>=(return.g)
