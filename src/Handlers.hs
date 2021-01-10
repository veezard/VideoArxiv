{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handlers where

import Control.Monad
import Data.FileEmbed (embedFile)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Import
import Text.Julius (RawJS(..))
import Text.Read (readMaybe)

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.
getFaviconR :: Handler TypedContent
getFaviconR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $
    TypedContent "image/x-icon" $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR =
  return $ TypedContent typePlain $ toContent $(embedFile "config/robots.txt")

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR =
  defaultLayout $
     --Include a file in the widget
   do $(widgetFile "home")

getTalkR :: Int -> Handler Html
getTalkR talkId
  --newtalk <-
    --runDB $ insert $ Talk Nothing Nothing "link" Nothing Nothing Nothing
 = do
  talk <- runDB $ get404 (toSqlKey $ fromIntegral talkId :: TalkId)
  let workshopIdEntry = talkWorkshopId talk
  let speakerIdEntry = talkSpeakerId talk
  let speakerNameEntry = talkSpeakerName talk
  let maybeSpeakerAndId = do
        speakerId <- speakerIdEntry
        speakerName <- speakerNameEntry
        return (speakerName, speakerId)
  maybeWorkshopAndId <-
    case workshopIdEntry of
      Nothing -> return Nothing
      Just workshopid -> do
        maybeWorkshop <- runDB $ get workshopid
        return $
          case maybeWorkshop of
            Nothing -> Nothing
            Just workshop -> Just (workshop, workshopid)
  defaultLayout $
     --Include a file in the widget
   do $(widgetFile "talk")

getSpeakerR :: Int -> Handler Html
getSpeakerR speakerId
  --speaker <- runDB $ get404 speakerId
 = do
  defaultLayout $
     --Include a file in the widget
   do $(widgetFile "speaker")

getWorkshopR :: Int -> Handler Html
getWorkshopR workshopId
  --workshop <- runDB $ get404 $ toSqlKey (fromIntegral workshopId )
 = do
  defaultLayout $
     --Include a file in the widget
   do $(widgetFile "workshop")

getSpeakersR :: Handler Html
getSpeakersR =
  defaultLayout $
     --Include a file in the widget
   do $(widgetFile "speakers")

getWorkshopsR :: Handler Html
getWorkshopsR = do
  maybePageNumber <- lookupGetParam "page"
  defaultLayout $
     --Include a file in the widget
   do $(widgetFile "workshops")
