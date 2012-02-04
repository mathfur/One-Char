{-# LANGUAGE OverloadedStrings, TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}

{-|

This is where all the routes and handlers are defined for your site. The
'app' function is the initializer that combines everything together and
is exported by this module.

-}

module Site
  ( app
  ) where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
import qualified Data.ByteString.Char8 as B
import           Application
import Text.Peggy
import Data.Maybe
import Data.List
import Control.Monad.State
import Control.Applicative

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Handler App App ()
index = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices =
        [ ("start-time",   startTimeSplice)
        , ("current-time", currentTimeSplice)
        ]


index2 :: Handler App App ()
index2 = ifTop $ heistLocal (bindSplices index2Splices) $ render "index2"
  where
    index2Splices = [("compiledSource", compiledSourceSplice 3)]

compiledSourceSplice :: Int -> Splice AppHandler
compiledSourceSplice n = do
    statement <- decodedParam "statement" 
    result <- (liftIO $ foo $ B.unpack statement)
    return $ [TextNode $ T.pack (B.unpack (statement) ++ " -> " ++ result)]
  where
    decodedParam p = fromMaybe "" <$> getParam p

foo :: String -> IO String
foo src = do
    case (parseString expr "" src) of
      Right e -> do
        result <- mapM expand [e]
        return ( src ++ "\t=>\t" ++ (intercalate "| " result) )
      Left e -> do
        return $ show e

------------------------------------------------------------------------------
-- | For your convenience, a splice which shows the start time.
startTimeSplice :: Splice AppHandler
startTimeSplice = do
    time <- lift $ gets _startTime
    return $ [TextNode $ T.pack $ show $ time]


------------------------------------------------------------------------------
-- | For your convenience, a splice which shows the current time.
currentTimeSplice :: Splice AppHandler
currentTimeSplice = do
    time <- liftIO getCurrentTime
    return $ [TextNode $ T.pack $ show $ time]


------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: Handler App App ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" (T.decodeUtf8 message)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",            index2)
         , ("/echo/:stuff", echo)
         , ("", with heist heistServe)
         , ("", serveDirectory "resources/static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    sTime <- liftIO getCurrentTime
    h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
    addRoutes routes
    return $ App h sTime



data Expr_ = Obj [(Char, Expr_)]
          | Func [Arg] Expr_
          | Sequence_ Target [Operation]
          deriving Show

data Operation = Operation Prefix [Expr_] deriving Show

data Arg = Arg { arg2Char :: Char, arg2String :: String } deriving Show

data Target = Target Char String deriving Show

data Prefix = Prefix Char String deriving Show

-- TODO:
-- * kvsなど対応
-- * a"isA"(b;c;)の展開
-- * e(v-ve)の展開
--  * -> each((v) -> v.e())
-- * e(.e)の展開
[peggy|
expr :: Expr_ = target operation+ { Sequence_ $1 $2 }

target :: Target = [a-z]+ [0-9] { Target $2 $1 }
operation :: Operation = prefix ('(' inner_parenthsis ')')? { Operation $1 (fromMaybe [] $2) }

inner_parenthsis :: [Expr_] = head_args ('>' tail_args)? { $1 ++ fromMaybe [] $2 }

head_args :: [Expr_] = target* { map (\t -> Sequence_ t []) $1 }
tail_args :: [Expr_] = second_args third_args func? { $1 ++ $2 ++ maybeToList $3 }

second_args :: [Expr_] = expr*
third_args :: [Expr_] = (' ' key ':' expr)* { if (null $1) then [] else [Obj $1] }
func :: Expr_ = ' ' func_args '-' expr { Func $1 $2 }
              / '.' expr           { Func [] $1 }

key :: Char = [a-z]
func_args :: [Arg] = func_arg*
func_arg :: Arg = [a-z]+ [0-9] { Arg $2 $1 }
prefix :: Prefix = [a-z]+ [0-9] { Prefix $2 $1 }
|]

expand :: Expr_ -> IO String
expand ( Obj pairs ) = do
  xss <-  mapM expandPair pairs
  return $ "{" ++ (intercalate ", " xss) ++ "}"
    where
      expandPair :: (Char, Expr_) -> IO String
      expandPair (c,expr) = do
        (\a b -> a ++ ": " ++ b) <$> (getWord [c]) <*> (expand expr)
expand (Func as expr) = do
  (\a b -> "function(" ++ a ++ "){ return (" ++ b ++ ") }") <$> (joinToString (intercalate ", ") expandArg as) <*> (expand expr)
    where
      expandArg :: Arg -> IO String
      expandArg = getWord.arg2String
expand (Sequence_ (Target d cs) os) = do
  c_ <- getWord cs
  os_ <- joinToString (intercalate ".") expandOper os
  return $ c_ ++ (if (null os_) then "" else ("." ++ os_))
    where
      expandOper :: Operation -> IO String
      expandOper (Operation (Prefix d cs) es) = do
        (++) <$> (getWord cs) <*> (joinToString (\xs -> "(" ++ (intercalate ", " xs) ++ ")") expand es)

getWord :: String -> IO String
getWord xs = do
  let env = [('e', "each"), ('m', "map"), ('s', "select"), ('f', "function"), ('h', "hash"), ('a', "array"), ('o', "object"),('d', "disable")]
  return $ fromMaybe xs $ lookup (head xs) env

joinToString :: ([String] -> String) -> (a -> IO String) -> [a] -> IO String
joinToString g f as = (mapM f as)>>=(return.g)
