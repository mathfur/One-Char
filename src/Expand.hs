{-# LANGUAGE OverloadedStrings, TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}

module Expand
  ( expandFromExpr
  ) where

import qualified Data.ByteString.Char8 as B
import           Control.Applicative
import           Data.Maybe
import           Data.List

import OneCharType
import Helpers

expandFromExpr :: Expr_ -> IO String
expandFromExpr ( Obj pairs ) = do
  xss <-  mapM expandPair pairs
  return $ "{" ++ (intercalate ", " xss) ++ "}"
    where
      expandPair :: (Key, Expr_) -> IO String
      expandPair (Key d cs,expr) = do
        (\a b -> a ++ " => " ++ b) <$> (getWordSpecifiedLength (Just $ read [d]) cs) <*> (expandFromExpr expr)
expandFromExpr (Func as exprs) = do
  args <- (joinToString (intercalate ", ") expandArg as)
  inners <- mapM expandFromExpr exprs
  let inner = intercalate ['\n'] inners
  return $ " do |" ++ args ++ "|\n" ++ inner ++ "\nend"
    where
      expandArg :: Arg -> IO String
      expandArg (Arg d cs) = getWordSpecifiedLength (Just $ read [d]) cs
expandFromExpr (Sequence_ (Target d cs) os) = do
  if (0 < length os && isSubstituteLike (last os))
    then do
      substitutee <- (mapM expandFromExpr $ exprsOfOperation $ last os)>>=(return . intercalate ", ")
      substituter <- expandFromExpr (Sequence_ (Target d cs) (init os))
      return $ substitutee ++ " " ++ translateSubstituteLike ( substituteLike (last os)) ++ " " ++ substituter
    else do
      os_ <- joinToString (intercalate "") expandOper os
      case cs of
        "@" -> return $ "self" ++ os_
        "#" -> case os_ of
           '.':remainder -> return remainder
           _ -> return os_
        _ -> do
          expandedTarget <- (getWordSpecifiedLength (Just $ read [d]) cs)
          return $ expandedTarget ++ os_
        where
          translateSubstituteLike :: String -> String
          translateSubstituteLike xs = case xs of
            "=+" -> "+="
            _ -> "="
          expandOper :: Operation -> IO String
          expandOper (Operation (Prefix d cs) es) = do
            case cs of
              "$" -> (joinToString (\xs -> "[" ++ (intercalate ", " xs) ++ "]") expandFromExpr es)
              _ -> do
                let (nonFunc, func) = getNonFuncAndFunc es
                expandedExprs <- mapM expandFromExpr nonFunc
                funcToStr <- maybe (return "") expandFromExpr $ func
                nonFuncToStr <- case (length nonFunc) of
                  0 -> return "" -- 引数が無いときは()を付けない
                  _ -> return $ "(" ++ (intercalate ", " expandedExprs) ++ ")"
                expandedPrefix <- (getWordSpecifiedLength (Just $ read [d]) cs)
                fs <- return $ expandedPrefix ++ nonFuncToStr ++ funcToStr
                return $ "." ++ fs
expandFromExpr (PlainText cs) = return cs

getWordSpecifiedLength :: (Maybe Int) -> String -> IO String
getWordSpecifiedLength l xs
  | (l == Just 0) = do
    -- TODO: バッファに保持する
    return xs
  | (l == Just 1 && (0 < length xs)) = return [head xs]
  | (l == Just 9 && (xs `elem` ["e", "s", "m"])) = case xs of
    "e" -> return "each"
    "s" -> return "select"
    "m" -> return "map"
  | otherwise = do
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

isSubstituteLike :: Operation -> Bool
isSubstituteLike (Operation (Prefix d cs) es) = (d == '9') && (cs `elem` ["=", "=+"])
