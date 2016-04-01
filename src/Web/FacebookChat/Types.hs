{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Web.FacebookChat.Types
    where

import BasicPrelude

import GHC.Generics (Generic)

import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson                (FromJSON, (.:), (.:?))
import qualified Data.Aeson.Lens  as Aeson

import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time.POSIX
import Data.Time.Clock (UTCTime)

import qualified Data.Scientific as Scientific

-- TODO get rid of this orphan instance
instance FromJSON (Time.NominalDiffTime) where
  parseJSON (Aeson.Number n) = return (realToFrac (Scientific.toRealFloat (n / 1000)))
  parseJSON _ = empty

data Gender = Unknown
            | Female_Singular
            | Male_Singular
            | Female_Singular_Guess
            | Male_Singular_Guess
            | Mixed
            | Neuter_Singular
            | Unknown_Singular
            | Female_Plural
            | Male_Plural
            | Neuter_Plural
            | Unknown_Plural
            deriving (Show, Enum)

instance FromJSON Gender where
  parseJSON (Aeson.Number n) =
    case Scientific.floatingOrInteger n of
      Right n -> return (toEnum n)
      Left  _ -> empty
  parseJSON _ = empty

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

data Friend = Friend
  { f_id :: Text
  , f_name :: Text
  , firstName :: Text
  , vanity :: Text
  , thumbSrc :: Text
  , uri :: Text
  , gender :: Gender
  , i18nGender :: Int
  , additionalName :: Maybe Text
  , f_type :: Text
  , is_friend :: Bool
  , mThumbSrcSmall :: Maybe Text
  , mThumbSrcLarge :: Maybe Text
  , dir :: Maybe Aeson.Value
  , searchTokens :: [Text]
  , alternateName :: Text
  , is_nonfriend_messenger_contact :: Bool
  } deriving (Show)

instance FromJSON Friend where
  parseJSON (Aeson.Object o) =
        Friend
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "firstName"
    <*> o .: "vanity"
    <*> o .: "thumbSrc"
    <*> o .: "uri"
    <*> o .: "gender"
    <*> o .: "i18nGender"
    <*> o .:? "additionalName"
    <*> o .: "type"
    <*> o .: "is_friend"
    <*> o .: "mThumbSrcSmall"
    <*> o .: "mThumbSrcLarge"
    <*> o .: "dir"
    <*> o .: "searchTokens"
    <*> o .: "alternateName"
    <*> o .: "is_nonfriend_messenger_contact"

  parseJSON _ = empty

data Status = Offline | Idle | Active | Mobile
            deriving (Show, Enum)

instance FromJSON Status where
  parseJSON (Aeson.Number n) =
    case Scientific.floatingOrInteger n of
      Right n -> return (toEnum n)
      Left  _ -> empty
  parseJSON _ = empty

type UserId = Text
