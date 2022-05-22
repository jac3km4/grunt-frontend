module Grunt.Api
  ( addSubscription
  , deleteSubscription
  , deleteUnread
  , getEntries
  , getStarred
  , getStarredEntries
  , getSubscriptions
  , getUnreadEntries
  , refreshSubscriptions
  , starEntries
  , unstarEntries
  ) where

import Prelude
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (URL, defaultRequest, request)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Grunt.Types (AppError(..), Entry, EntryId, FeedId, Subscription)
import Safe.Coerce (coerce)
import Web.HTML as HTML
import Web.HTML.Location as Location
import Web.HTML.Window as Window

refreshSubscriptions :: Aff (Either AppError Unit)
refreshSubscriptions = do
  uri <- constructUri "/admin/jobs/refresh"
  makeReq POST uri Nothing ignoreFormat

getSubscriptions :: Aff (Either AppError (Array Subscription))
getSubscriptions = do
  uri <- constructUri "/feedbin/subscriptions.json"
  makeReq GET uri Nothing jsonFormat

addSubscription :: URL -> Aff (Either AppError Subscription)
addSubscription feedUrl = do
  uri <- constructUri "/feedbin/subscriptions.json"
  let
    body = RB.json $ encodeJson { feed_url: feedUrl }
  makeReq POST uri (Just body) jsonFormat

deleteSubscription :: FeedId -> Aff (Either AppError Unit)
deleteSubscription feedId = do
  uri <- constructUri $ "/feedbin/subscriptions/" <> show feedId <> ".json"
  let
    body = RB.json $ encodeJson { feed_id: feedId }
  makeReq DELETE uri (Just body) ignoreFormat

getEntries :: { page :: Int, perPage :: Int } -> Aff (Either AppError (Array Entry))
getEntries { page, perPage } = do
  uri <- constructUri $ "/feedbin/entries.json?page=" <> show page <> "&per_page=" <> show perPage
  makeReq GET uri Nothing jsonFormat

getStarred :: { page :: Int, perPage :: Int } -> Aff (Either AppError (Array Entry))
getStarred { page, perPage } = do
  uri <- constructUri $ "/feedbin/entries.json?starred=true&page=" <> show page <> "&per_page=" <> show perPage
  makeReq GET uri Nothing jsonFormat

getStarredEntries :: Aff (Either AppError (Array EntryId))
getStarredEntries = do
  uri <- constructUri "/feedbin/starred_entries.json"
  makeReq GET uri Nothing jsonFormat

starEntries :: Array EntryId -> Aff (Either AppError Unit)
starEntries entries = do
  uri <- constructUri "/feedbin/starred_entries.json"
  let
    body = RB.json $ encodeJson { starred_entries: entries }
  makeReq POST uri (Just body) ignoreFormat

unstarEntries :: Array EntryId -> Aff (Either AppError Unit)
unstarEntries entries = do
  uri <- constructUri "/feedbin/starred_entries.json"
  let
    body = RB.json $ encodeJson { starred_entries: entries }
  makeReq DELETE uri (Just body) ignoreFormat

getUnreadEntries :: Aff (Either AppError (Array EntryId))
getUnreadEntries = do
  uri <- constructUri "/feedbin/unread_entries.json"
  makeReq GET uri Nothing jsonFormat

deleteUnread :: Array EntryId -> Aff (Either AppError Unit)
deleteUnread entries = do
  uri <- constructUri "/feedbin/unread_entries.json"
  let
    body = RB.json $ encodeJson { unread_entries: entries }
  makeReq DELETE uri (Just body) ignoreFormat

makeReq :: forall i res. Method -> URL -> Maybe RB.RequestBody -> Format i res -> Aff (Either AppError res)
makeReq method url body (Format format process onError) = do
  res <- request (defaultRequest { method = Left method, url = url, content = body, responseFormat = format })
  pure case res of
    Right resp
      | coerce resp.status == 200 || coerce resp.status == 201 -> process resp.body
      | otherwise -> Left $ onError resp.body resp.status
    Left err -> Left $ HttpError err

constructUri :: forall m. MonadEffect m => String -> m String
constructUri path = do
  baseUri <- liftEffect $ HTML.window >>= Window.location >>= Location.origin
  pure $ baseUri <> path

data Format a b
  = Format (RF.ResponseFormat a) (a -> Either AppError b) (a -> StatusCode -> AppError)

jsonFormat :: forall a. DecodeJson a => Format Json a
jsonFormat = Format RF.json (lmap JsonError <<< decodeJson) (const <<< either JsonError mapError <<< decodeJson)
  where
  mapError :: { message :: String } -> AppError
  mapError { message } = ApiError message

ignoreFormat :: Format Unit Unit
ignoreFormat = Format RF.ignore Right (const UnexpectedResponse)
