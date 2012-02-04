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
    result <- (liftIO $ expand $ B.unpack statement)
    return $ [TextNode $ T.pack result]
  where
    decodedParam p = fromMaybe "" <$> getParam p

expand :: String -> IO String
expand src = do
    case (mainParser src) of
      Right e -> expandFromExpr e
      Left e -> return $ show e
      -- TODO: eitherでまとめられる?

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

