{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module ResponseTypes
    where

import BasicPrelude

import GHC.Generics (Generic)

import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson                (FromJSON, (.:))
import qualified Data.Aeson.Lens  as Aeson

import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time.POSIX
import Data.Time.Clock (UTCTime)

import qualified Data.Scientific as Scientific

data Response a = Response
  { __ar :: Int
  , payload :: a
  , bootloadable :: Aeson.Object
  , ixData :: Aeson.Object
  , lid :: Text
  } deriving Show

instance FromJSON (Time.NominalDiffTime) where
  parseJSON (Aeson.Number n) = return (realToFrac (Scientific.toRealFloat n))
  parseJSON invalid = Aeson.typeMismatch "NominalDiffTime" invalid

parseResponse :: FromJSON a => Text -> (Aeson.Value -> Aeson.Parser (Response a))
parseResponse name = parser
  where
    parser (Aeson.Array _) = empty -- fail on array
    parser (Aeson.Object o) =
          Response
      <$> o .: "__ar"
      <*> ((o .: "payload") >>= (.: name))
      <*> o .: "bootloadable"
      <*> o .: "ixData"
      <*> o .: "lid"

data Thread = Thread
  { thread_id :: Text
  , thread_fbid :: Text
  , other_user_fbid :: Maybe Text
  , participants :: [Text]
  , former_participants :: [Aeson.Value]
  , name :: Text
  , snippet :: Text
  , snippet_has_attachment :: Bool
  , snippet_attachments :: [Aeson.Value]
  , unread_count :: Int
  , message_count :: Int
  , image_src :: Maybe Text
  , timestamp_absolute :: Text
  , timestamp_datetime :: Text
  , timestamp_relative :: Text
  , timestamp_time_passed :: Time.POSIX.POSIXTime
  , timestamp :: Time.POSIX.POSIXTime
  , server_timestamp :: Time.POSIX.POSIXTime
  , mute_settings :: Aeson.Value
  , is_canonical_user :: Bool
  , is_canonical :: Bool
  , is_subscribed :: Bool
  , folder :: Text
  , is_archived :: Bool
  , recipients_loadable :: Bool
  , name_conversation_sheet_dismissed :: Bool
  , has_email_participant :: Bool
  , read_only :: Bool
  , can_reply :: Bool
  , composer_enabled :: Bool
  , last_message_timestamp :: Time.POSIX.POSIXTime
  , last_read_timestamp :: Time.POSIX.POSIXTime
  , last_message_type :: Text
  , ephemeral_ttl_mode :: Int
  , custom_like_icon :: Maybe (HashMap Text Text)
  , titan_originated_thread_id :: Text
  , custom_nickname :: Maybe (HashMap Text Text)
  , custom_color :: Maybe Text
  , admin_ids :: [Text]
  } deriving (Show, Generic, Aeson.FromJSON)