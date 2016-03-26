{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    where

import BasicPrelude
import qualified Control.Applicative as Applicative
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Control.Exception.Base as Exception

import qualified Control.Monad.State as State
import Control.Monad.State (State, StateT)
import qualified Control.Monad.Catch as Catch

import qualified Data.Text.Encoding as Encoding
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified System.Random as Random

import qualified Network.Wreq.Session as Wreq.Session
import qualified Network.Wreq as Wreq
import Network.Wreq (FormParam((:=)))

import Control.Lens ((&), (.~), (^.), (^..), (^?), (^?!), over, _Left)

import qualified Numeric
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time.POSIX

import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson                (FromJSON)
import qualified Data.Aeson.Lens  as Aeson

import qualified Text.HTML.DOM as HTML
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML

import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lens as ByteString
import qualified Data.ByteString      as ByteString
import qualified Data.Text.Lens as Text

import qualified ResponseTypes as ResponseTypes

import Debug.Trace

newtype FBException = FBException String deriving Show
instance Exception FBException

parseJson :: Catch.MonadThrow m => LByteString -> m Aeson.Value
parseJson str = if first == prefix
                  then decode rest
                  else Catch.throwM (FBException ("expected" <> textToString (show prefix) <> ", but found " <> textToString (show rest)))
  where prefix = "for (;;);"
        (first, rest) = ByteString.Lazy.splitAt (ByteString.Lazy.length prefix) str

encode = Encoding.encodeUtf8

-- loginURL, searchURL, sendURL, threadsURL, threadSyncURL, messagesURL, readStatusURL, deliveredURL, markSeenURL, baseURL, mobileURL, stickyURL, pingURL :: Text
loginURL      = "https://m.facebook.com/login.php?login_attempt=1"
searchURL     = "https://www.facebook.com/ajax/typeahead/search.php"
sendURL       = "https://www.facebook.com/ajax/mercury/send_messages.php"
threadsURL    = "https://www.facebook.com/ajax/mercury/threadlist_info.php"
threadSyncURL = "https://www.facebook.com/ajax/mercury/thread_sync.php"
messagesURL   = "https://www.facebook.com/ajax/mercury/thread_info.php"
readStatusURL = "https://www.facebook.com/ajax/mercury/change_read_status.php"
deliveredURL  = "https://www.facebook.com/ajax/mercury/delivery_receipts.php"
markSeenURL   = "https://www.facebook.com/ajax/mercury/mark_seen.php"
baseURL       = "https://www.facebook.com"
mobileURL     = "https://m.facebook.com/"
stickyURL     = "https://0-edge-chat.facebook.com/pull"
pingURL       = "https://0-channel-proxy-06-ash2.facebook.com/active_ping"

userAgent :: Text
userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.90 Safari/537.36"

client :: Text
client = "mercury"

data Authentication = Authentication
  { username :: Text
  , password :: Text
  }

data FBSession = FBSession
  { session :: Wreq.Session.Session
  , requestCounter :: Int
  , seq' :: Text
  , sessionUserAgent :: ByteString
  , prev :: Time.UTCTime
  , tmpPrev :: Time.UTCTime
  , lastSync :: Time.UTCTime
  , payloadDefault :: Params
  , ttStamp :: Text
  , uid :: Integer
  , userChannel :: Text
  , clientId :: Text
  }

data Header = Header
  { contentType :: ByteString
  , referer :: ByteString
  , origin :: ByteString
  , headerUserAgent :: ByteString
  , connection :: ByteString
  }

data Payload = Payload
  { rev :: Text
  , user :: Text
  , a :: Text
  , ttstamp :: Text
  , fb_dtsg :: Text
  }

defaultHeader :: Header
defaultHeader = Header
  { contentType = "application/x-www-form-urlencoded"
  , referer = encode baseURL
  , origin = encode baseURL
  , headerUserAgent = encode userAgent
  , connection = "keep-alive"
  }

headerOptions :: Header -> Wreq.Options -> Wreq.Options
headerOptions h = foldr1 (.) updates
  where
    updates :: [Wreq.Options -> Wreq.Options]
    updates = [ Wreq.header "Referer"      .~ [referer h]
              , Wreq.header "Origin"       .~ [origin h]
              , Wreq.header "User-Agent"   .~ [headerUserAgent h]
              , Wreq.header "Connection"   .~ [connection h]
              ]

type Params = [(Text, Text)]

post' :: String -> Params -> StateT FBSession IO (Wreq.Response LByteString)
post' url params = do
  fbState <- State.get
  payload <- generatePayload params
  State.liftIO $ Wreq.Session.post (session fbState) url (map paramToFormParam payload)

  where
    paramToFormParam :: (Text, Text) -> FormParam
    paramToFormParam (name, value) = (encode name) := (encode value)

get' :: String -> Params -> StateT FBSession IO (Wreq.Response LByteString)
get' url params = do
  fbState <- State.get
  payload <- generatePayload params
  let opts = Wreq.defaults & foldr1 (.) (map toParam payload)
  State.liftIO $ Wreq.Session.getWith opts (session fbState) url
  where
    toParam (name, value) = Wreq.param name .~ [value]

-- Not sure that this function should have side effects (incrementing the
-- request counter). Maybe this should be done in get' and post'.
generatePayload :: Monad m => Params -> StateT FBSession m Params
generatePayload params = do
  fbState <- State.get
  let count' = (requestCounter fbState) + 1

      encodeDigit x
        | 0  <= x && x < 10 = Char.chr (x + Char.ord '0')
        | 10 <= x && x < 36 = Char.chr ((x - 10) + Char.ord 'a')

      params' = (payloadDefault fbState) ++ params
                ++ [("__req", Text.pack $ Numeric.showIntAtBase 36 encodeDigit count' "")]

  State.put $ fbState { requestCounter = count' }
  return params'

getUserId :: Text -> StateT FBSession IO (Wreq.Response LByteString)
getUserId query = do
  uid_ <- uid <$> State.get
  let
    form =
      [ ("value", query)
      , ("viewer", show uid_)
      , ("rsp", "search")
      , ("context", "search")
      , ("path", "/home.php")
      , ("request_id", "1304")
      ]
  get' "https://www.facebook.com/ajax/typeahead/search.php" form

-- TODO accept start/end arguments
getThreadList :: Int -> Int -> StateT FBSession IO (ResponseTypes.Response [ResponseTypes.Thread])
getThreadList start end = do
  let
    form =
      [ ("client",        client)
      , ("inbox[offset]", show start)
      , ("inbox[limit]",  show (end - start))
      ]

  r <- post' threadsURL form
  json <- parseJson (r ^. Wreq.responseBody)

  parse (ResponseTypes.parseResponse "threads") json

getFriendsList :: StateT FBSession IO (HashMap Text ResponseTypes.Friend)
getFriendsList = do
  uid_ <- uid <$> State.get
  let form = [("viewer", show uid_)]

  response <- post' "https://www.facebook.com/chat/user_info_all" form

  json :: Aeson.Value <- parseJson (response ^. Wreq.responseBody)
  res_ <- maybeToError (json ^? Aeson.key "payload")
  res <- parse Aeson.parseJSON res_

  return res

getOnlineUsers :: StateT FBSession IO (HashMap ResponseTypes.UserId ResponseTypes.Status, HashMap ResponseTypes.UserId Integer)
getOnlineUsers = do
  uid_ <- uid <$> State.get
  -- TODO specify this using Bool type instead of strings
  let form = [ ("user", show uid_)
             , ("fetch_mobile", "false")
             , ("get_now_available_list", "true") ]
  response <- post' "https://www.facebook.com/ajax/chat/buddy_list.php" form

  json :: Aeson.Value <- parseJson (response ^. Wreq.responseBody)
  buddyList <- maybeToError (json ^? Aeson.key "payload" . Aeson.key "buddy_list")

  nowAvailableList <- maybeToError (buddyList ^? Aeson.key "nowAvailableList")
  nowAvailableList' <- parse Aeson.parseJSON (over Aeson.members (^?! Aeson.key "a") nowAvailableList)

  lastActiveTimes <- maybeToError (buddyList ^? Aeson.key "last_active_times" >>= Aeson.parseMaybe Aeson.parseJSON)

  return (nowAvailableList', lastActiveTimes)

eitherToError :: Catch.MonadThrow m => Either String b -> m b
eitherToError = either (Catch.throwM . FBException) return

maybeToError :: Catch.MonadThrow m => Maybe b -> m b
maybeToError = maybe (Catch.throwM (FBException "was Maybe")) return

decode :: (FromJSON a, Catch.MonadThrow m) => LByteString -> m a
decode = eitherToError . Aeson.eitherDecode

parse :: (FromJSON a, Catch.MonadThrow m) => (a -> Aeson.Parser b) -> a -> m b
parse f = eitherToError . (Aeson.parseEither f)

timeToMilliseconds :: Integral a => Time.UTCTime -> a
timeToMilliseconds time = round ((Time.POSIX.utcTimeToPOSIXSeconds time) * 1000)

responseToXML :: Wreq.Response LByteString -> XML.Cursor
responseToXML response = (XML.fromDocument . HTML.parseLBS) (response ^. Wreq.responseBody)

login :: Wreq.Session.Session -> Authentication -> IO (Maybe FBSession)
login session (Authentication username password) = do
  loginPage <- Wreq.Session.get session mobileURL

  let
    doc = responseToXML loginPage
    inputs = (doc XML.$.// XML.attributeIs "id" "login_form") >>= (XML.$.// XML.element "input")
    formUrl = doc XML.$.// XML.attributeIs "id" "login_form" >=> XML.attribute "action"

    pairs :: [FormParam]
    pairs = [   (encode $ concat (XML.attribute "name"  cursor))
             := (encode $ concat (XML.attribute "value" cursor))
            | cursor <- inputs ]

    additionalPairs = [ "email" := username
                      , "pass"  := password
                      , "login" := ("Log In" :: ByteString)
                      ]

    -- additionalPairs entries take precedence over pairs entries
    payload = pairs ++ additionalPairs

    opts = Wreq.defaults & headerOptions defaultHeader

  loginResponse <- Wreq.Session.postWith opts session (textToString $ concat formUrl) payload

  let
    uidMaybe :: Maybe Integer
    uidMaybe = (loginResponse ^? Wreq.responseCookie "c_user" . Wreq.cookieValue) >>= (readMay . Encoding.decodeUtf8)

  case uidMaybe of
    Nothing -> return Nothing

    Just uid -> do
      clientId <- Text.pack . flip Numeric.showHex "" <$> (Random.randomIO :: IO Int)
      now <- Time.getCurrentTime
      let
        startTime = timeToMilliseconds now

        userChannel = "p_" <> show uid

        doc' = responseToXML loginResponse

        fb_dtsg = concat $ doc' XML.$.// (XML.attributeIs "name" "fb_dtsg" >=> XML.attribute "value")

        ttStamp :: Text
        ttStamp = concat [show (Char.ord c) <> "2" | c <- Text.unpack fb_dtsg]

        payloadDefault :: Params
        payloadDefault =
          [ ("__rev",   "2246636")
          , ("__user",  show uid)
          , ("__a",     "1")
          , ("ttstamp", ttStamp)
          , ("fb_dtsg", fb_dtsg)
          ]

        prev     = now
        tmpPrev  = now
        lastSync = now

        fbSession = FBSession
          { session = session
          , requestCounter = 1
          , seq' = ""
          , sessionUserAgent = encode userAgent
          , prev = prev
          , tmpPrev = tmpPrev
          , lastSync = lastSync
          , payloadDefault = payloadDefault
          , ttStamp = ttStamp
          , uid = uid
          , userChannel = userChannel
          , clientId = clientId
          }

      return (Just fbSession)
