{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Handlers where

import Control.Monad.Trans.Maybe
import Data.FileEmbed (embedFile)
import Data.Function ((&))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Import
import Prelude ((!!), fromIntegral)
import Text.Julius (RawJS(..))
import Text.Read (readMaybe)
import Yesod.Paginator
import Yesod.Paginator.Pages (pageNumber)

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
getHomeR = do
  searchQueryMaybe <- lookupGetParam "query"
  let searchQuery = fromMaybe "" searchQueryMaybe
  let perPage = 20
  pages <-
    if searchQuery /= ""
      then runDB $
           selectPaginated
             perPage
             ([match TalkSpeakerName searchQuery] ||.
              [match TalkTitle searchQuery] ||.
              [match TalkAbstract searchQuery])
             []
      else runDB $ selectPaginated perPage [] []
  let page = pagesCurrent pages :: Page (Entity Talk)
  let enumStart = fromIntegral (pageNumber page - 1) * fromIntegral perPage + 1
  defaultLayout $ do $(widgetFile "home")

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
    runMaybeT $ do
      workshopid <- MaybeT $ return workshopIdEntry
      workshop <- MaybeT $ runDB $ get workshopid
      return (workshop, workshopid)
  defaultLayout $ do $(widgetFile "talk")

getSpeakerR :: Int -> Handler Html
getSpeakerR requestSpeakerId = do
  speaker <-
    runDB $ get404 (toSqlKey $ fromIntegral requestSpeakerId :: SpeakerId)
  let speakerFullName =
        speakerFirstName speaker ++ " " ++ speakerLastName speaker
  let perPage = 20
  pages <-
    runDB $
    selectPaginated
      perPage
      [TalkSpeakerId ==. Just (toSqlKey $ fromIntegral requestSpeakerId)]
      []
  let page = pagesCurrent pages :: Page (Entity Talk)
  let enumStart = fromIntegral (pageNumber page - 1) * fromIntegral perPage + 1
  defaultLayout $ do $(widgetFile "speaker")

getWorkshopR :: Int -> Handler Html
getWorkshopR requestWorkshopId = do
  workshop <-
    runDB $ get404 (toSqlKey $ fromIntegral requestWorkshopId :: WorkshopId)
  let perPage = 20
  pages <-
    runDB $
    selectPaginated
      perPage
      [TalkWorkshopId ==. Just (toSqlKey $ fromIntegral requestWorkshopId)]
      []
  let page = pagesCurrent pages :: Page (Entity Talk)
  let enumStart = fromIntegral (pageNumber page - 1) * fromIntegral perPage + 1
  defaultLayout $ do $(widgetFile "workshop")

getSpeakersR :: Handler Html
getSpeakersR = do
  let perPage = 20
  pages <-
    runDB $
    selectPaginated perPage [] [Asc SpeakerLastName, Asc SpeakerFirstName]
  let page = pagesCurrent pages :: Page (Entity Speaker)
  let enumStart = fromIntegral (pageNumber page - 1) * fromIntegral perPage + 1
  defaultLayout $ do $(widgetFile "speakers")

getWorkshopsR :: Handler Html
getWorkshopsR = do
  let perPage = 20
  pages <- runDB $ selectPaginated perPage [] []
  let page = pagesCurrent pages :: Page (Entity Workshop)
  let enumStart = fromIntegral (pageNumber page - 1) * fromIntegral perPage + 1
  defaultLayout $ do $(widgetFile "workshops")

insertWorkshop :: Text -> Handler ()
insertWorkshop workshopName = do
  _ <- runDB $ insert $ Workshop workshopName
  return ()

showWorkshop :: Entity Workshop -> Widget
showWorkshop workshopEnt =
  [whamlet|
<li>
  <a href=@{WorkshopR $ dbIdFromEntity workshopEnt}> 
    #{workshopTitle $ entityVal workshopEnt}
|]

showSpeaker :: Entity Speaker -> Widget
showSpeaker speakerEnt =
  [whamlet|
<li>
  <a href=@{SpeakerR $ dbIdFromEntity speakerEnt}>
    #{speakerLastName $ entityVal speakerEnt}, #{speakerFirstName $ entityVal speakerEnt}
|]

showTalk :: Entity Talk -> Widget
showTalk talkEnt =
  [whamlet|
<li title="#{abstract}">
    <div .formSpecs>
    <a href=@{TalkR $ dbIdFromEntity talkEnt}>
      Title: #
      $maybe title <- talkTitle talk
        <div .formData>
          #{title}
        <br>
      $nothing
        <br>
  $maybe speakerAndId <- maybeSpeakerAndId 
    <div .formSpecs>
      Speaker: #
    <div .formData>
      <a href=@{SpeakerR $ fromIntegral $ fromSqlKey $ snd speakerAndId}>
        #{fst speakerAndId}
    <br>
  $nothing
    <div>
  <div .formSpecs>
    Link: #
  <div .formData>
    <a href="#{talkLink talk}">
      #{talkLink talk}
  <br>
|]
  where
    talk = entityVal talkEnt
    abstract = "" `fromMaybe` talkAbstract talk :: Text
    speakerIdEntry = talkSpeakerId talk
    speakerNameEntry = talkSpeakerName talk
    maybeSpeakerAndId = do
      speakerId <- speakerIdEntry
      speakerName <- speakerNameEntry
      return (speakerName, speakerId)

withIndices :: forall a. [a] -> [(a, Int)]
withIndices list = [(x, i) | i <- [1 .. length list], let x = list !! (i - 1)]

dbIdFromEntity ::
     forall record. ToBackendKey SqlBackend record
  => Entity record
  -> Int
dbIdFromEntity = fromIntegral . fromSqlKey . entityKey

-- | Implements the `match` operator. This operator is specific to Sqlite3 and
--   is used to look for keywords in FTS3/4/5 tables.
match ::
     EntityField record (Maybe Text) -- ^ Field to filter on
  -> Text -- ^ Text to compare with
  -> Filter record -- ^ Resulting filter
match field val = Filter field (Left $ Just val) (BackendSpecificFilter "match")

ellipsed' :: Int -> Pages a -> WidgetFor m ()
ellipsed' num pages =
  if (pages & pagesCurrent & pageItems & length) == 0
    then [whamlet|<div>|]
    else ellipsed (fromIntegral num) pages
