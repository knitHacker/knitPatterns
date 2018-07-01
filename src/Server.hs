{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Server where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid
import Lucid.Base

import CommandLineInterface
import KnitStitches

import Data.Map as M

import Control.Monad.Trans
import Control.Monad
import Data.Monoid
import Data.IORef
import qualified Data.Text as T

data MySession = EmptySession
data MyAppState = DummyAppState (IORef (Map String Pattern))

rootHtml = lucid $ do
                head_ (title_ "Lianne's Knit Program")
                h1_ "Welcome to knitPatterns!"
                ul_ $ forM_ options $ \option -> li_ $ do
                    a_ [href_ (T.append "/" (T.pack option))] $ toHtml option

stitchesHtml = lucid $ do
                head_ (title_ "Lianne's Knit Program")
                h1_ "Supported Stitches"
                forM_ stitchDescriptions $ \desc -> p_ $ do
                    toHtml desc

inputHtml = lucid $ do
                head_ (title_ "Lianne's Knit Program")
                h1_ "Not supported yet"


listHtml patternNames = lucid $ do
                head_ (title_ "Lianne's Knit Program")
                h1_ "List of Available Patterns"
                forM_ patternNames $ \name -> p_ $ do
                    toHtml name

showHtml = lucid $ do
                head_ (title_ "Lianne's Knit Program")
                h1_ "Not supported yet"

initApp :: IO ()
initApp =
    do ref <- newIORef (M.empty)
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
        (DummyAppState ref) <- getState
        do get root $ rootHtml
           get "Stitches" $ stitchesHtml
           get "Input" $ inputHtml
           get "List" $ do
                pMap <- liftIO $ readIORef ref
                listHtml (M.keys pMap)
           get "Show" $ showHtml
