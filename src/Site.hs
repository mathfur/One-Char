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

import           Expand
import           OneCharType
import           Parser

index :: Handler App App ()
index = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices = [("compiledSource", compiledSourceSplice)]

compiledSourceSplice :: Splice AppHandler
compiledSourceSplice = do
    statement <- decodedParam "statement"
    result <- (liftIO $ expand $ B.unpack statement)
    let outputStr = either (\s -> (B.unpack statement) ++ " e") id result
    --return $ [TextNode $ T.pack $ ">>'" ++ (B.unpack statement) ++ "' -> '" ++ result ++ "'<<"]
    return $ [TextNode $ T.pack outputStr]
  where
    decodedParam p = fromMaybe "" <$> getParam p

expand :: String -> IO (Either String String)
expand src = do
    case (mainParser src) of
      Right e -> (expandFromExpr e)>>=(return.Right)
      Left e -> return $ Left $ show e
      -- TODO: eitherでまとめられる?

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",            index)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    sTime <- liftIO getCurrentTime
    h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
    addRoutes routes
    return $ App h sTime

