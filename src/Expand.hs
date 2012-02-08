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
      expandPair :: (Key, Expr_) -> IO String
      expandPair (Key d cs,expr) = do
        (\a b -> a ++ ": " ++ b) <$> (getWordSpecifiedLength (Just $ read [d]) cs) <*> (expandFromExpr expr)
expandFromExpr (Func as expr) = do
  (\a b -> "function(" ++ a ++ "){ return (" ++ b ++ ") }") <$> (joinToString (intercalate ", ") expandArg as) <*> (expandFromExpr expr)
    where
      expandArg :: Arg -> IO String
      expandArg (Arg d cs) = getWordSpecifiedLength (Just $ read [d]) cs
expandFromExpr (Sequence_ (Target d cs) os) = do
  os_ <- joinToString (intercalate "") expandOper os
  case cs of
    "@" -> return $ "self" ++ os_
    "_" -> case os_ of
       '.':remainder -> return remainder
       _ -> return os_
    _ -> do
      expandedTarget <- (getWordSpecifiedLength (Just $ read [d]) cs)
      return $ expandedTarget ++ os_
    where
      expandOper :: Operation -> IO String
      expandOper (Operation (Prefix d cs) es) = do
        case cs of
          "$" -> (joinToString (\xs -> "[" ++ (intercalate ", " xs) ++ "]") expandFromExpr es)
          _ -> do
            expandedExprs <- mapM expandFromExpr es
            inner <- case (length expandedExprs) of
              0 -> return ""
              _ -> return $ "(" ++ (intercalate ", " expandedExprs) ++ ")"
            expandedPrefix <- (getWordSpecifiedLength (Just $ read [d]) cs)
            fs <- return $ expandedPrefix ++ inner
            return $ "." ++ fs
expandFromExpr (PlainText cs) = return cs

getWordSpecifiedLength :: (Maybe Int) -> String -> IO String
getWordSpecifiedLength l xs = do
  dicWords <- (B.readFile "dic/dic.txt">>=(return.(B.split '\n')))
  print $ xs ++ " -> '" ++ (B.unpack $ candidate dicWords) ++ "' #dicWords: " ++ show (length dicWords) ++ " #candidates:" ++ (show $ length $ candidates dicWords)
  return $ B.unpack (candidate dicWords)
    where
      candidate as = if (0 == length (candidates as)) then "" else (head (candidates as))
      candidates as = filter (\ys -> ((isNothing l) || (B.length ys == (fromJust l))))
        $ filter (isIncludedByMeaningOfEachWord $ B.pack xs)
        $ filter (haveSameHead (B.pack xs)) $ as

-- 例) as = 'intzn', bs = 'internationalization' -> true
isIncludedByMeaningOfEachWord :: B.ByteString -> B.ByteString -> Bool
isIncludedByMeaningOfEachWord as bs
  | (B.length as == 0) = True
  | (B.length bs == 0) = False
  | (B.length bs == B.length bs') = False
  | otherwise = (isIncludedByMeaningOfEachWord (B.tail as) bs')
    where
      bs' = snd $ B.breakEnd (\c -> (B.head as == c)) bs

haveSameHead :: B.ByteString -> B.ByteString -> Bool
haveSameHead as bs
  | (B.null as && B.null bs) = True
  | (B.null as || B.null bs) = False
  | otherwise = (B.head as == B.head bs)

joinToString :: ([String] -> String) -> (a -> IO String) -> [a] -> IO String
joinToString g f as = (mapM f as)>>=(return.g)
