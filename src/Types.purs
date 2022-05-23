module Grunt.Types where

import Prelude

import Affjax.StatusCode (StatusCode)
import Affjax.Web as Web
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson)
import Data.JSDate as JSDate
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Effect.Unsafe (unsafePerformEffect)


data AppError
  = HttpError Web.Error
  | UnexpectedResponse StatusCode
  | ApiError String
  | JsonError JsonDecodeError

instance Show AppError where
  show (HttpError err) = "http error: " <> Web.printError err
  show (UnexpectedResponse code) = "unexpected http status: " <> show code
  show (ApiError err) = err
  show (JsonError err) = "json error:" <> show err

newtype FeedId
  = FeedId Int

derive newtype instance Eq FeedId
derive newtype instance Ord FeedId
derive newtype instance Show FeedId
derive newtype instance DecodeJson FeedId
derive newtype instance EncodeJson FeedId

newtype EntryId
  = EntryId Number

derive newtype instance Eq EntryId
derive newtype instance Ord EntryId
derive newtype instance DecodeJson EntryId
derive newtype instance EncodeJson EntryId

newtype TaggingId
  = TaggingId Int

derive newtype instance Eq TaggingId
derive newtype instance Ord TaggingId
derive newtype instance Show TaggingId
derive newtype instance DecodeJson TaggingId
derive newtype instance EncodeJson TaggingId

newtype Timestamp
  = Timestamp String

derive instance Newtype Timestamp _

instance DecodeJson Timestamp where
  decodeJson json = do
    str <- decodeJson json
    pure
      $ Timestamp
      $ JSDate.toUTCString
      $ unsafePerformEffect
      $ JSDate.parse str

newtype Subscription
  = Subscription
  { id :: FeedId
  , createdAt :: Timestamp
  , feedId :: FeedId
  , title :: String
  , feedUrl :: String
  , siteUrl :: String
  }

derive instance Newtype Subscription _

instance DecodeJson Subscription where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    createdAt <- obj .: "created_at"
    feedId <- obj .: "feed_id"
    title <- obj .: "title"
    feedUrl <- obj .: "feed_url"
    siteUrl <- obj .: "site_url"
    pure $ Subscription { id, createdAt, feedId, title, feedUrl, siteUrl }

newtype Entry
    = Entry
    { id :: EntryId
    , feedId :: FeedId
    , title :: Maybe String
    , url :: String
    , author :: Maybe String
    , content :: Maybe String
    , summary :: Maybe String
    , published :: Timestamp
    , createdAt :: Timestamp
    , imageUrl :: Maybe String
    }

instance DecodeJson Entry where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    feedId <- obj .: "feed_id"
    title <- obj .:? "title"
    url <- obj .: "url"
    author <- obj .:? "author"
    content <- obj .:? "content"
    summary <- obj .:? "summary"
    published <- obj .: "published"
    createdAt <- obj .: "created_at"
    images <- obj .:? "images"
    imageUrl <- traverse (_ .: "original_url") images
    pure $ Entry { id, feedId, title, url, author, content, summary, published, createdAt, imageUrl } 

newtype Tagging
    = Tagging
    { id :: TaggingId
    , feedId :: FeedId
    , name :: Tag
    }

instance DecodeJson Tagging where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    feedId <- obj .: "feed_id"
    name <- obj .: "name"
    pure $ Tagging { id, feedId, name } 

newtype Tag
  = Tag String

derive newtype instance Eq Tag
derive newtype instance Ord Tag
derive newtype instance Show Tag
derive newtype instance DecodeJson Tag
derive newtype instance EncodeJson Tag
