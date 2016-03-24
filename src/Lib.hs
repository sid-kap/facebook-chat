{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lib
    -- ( someFunc
    -- ) where
    where

import BasicPrelude
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe

import qualified Control.Monad.State as State
import Control.Monad.State (State, StateT)

import qualified Data.Text.Encoding as Encoding
import qualified Data.Text as Text (pack)
import qualified Data.HashMap.Strict as HashMap
import qualified System.Random as Random

import qualified Network.Wreq.Session as Wreq.Session
import qualified Network.Wreq as Wreq
import Network.Wreq (FormParam((:=)))

import Lens.Family ((&), (.~), (^.), (^..), (^?), over)

import qualified Numeric
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time.POSIX

-- Conversion of Haskell values to JSON.
import qualified Data.Aeson      as Aeson
import qualified Data.Aeson.Lens as Aeson

import qualified Text.HTML.DOM as HTML
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML

-- Easy traversal of JSON data.
-- import Data.Aeson.Lens (key, nth)
-- import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lens as ByteString
import qualified Data.Text.Lens as Text

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

userAgents :: [Text]
userAgents =
  [ "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.90 Safari/537.36"
  , "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/601.1.10 (KHTML, like Gecko) Version/8.0.5 Safari/601.1.10"
  , "Mozilla/5.0 (Windows NT 6.3; WOW64; ; NCT50_AAP285C84A1328) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.90 Safari/537.36"
  , "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/22.0.1207.1 Safari/537.1"
  , "Mozilla/5.0 (X11; CrOS i686 2268.111.0) AppleWebKit/536.11 (KHTML, like Gecko) Chrome/20.0.1132.57 Safari/536.11"
  , "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.6 (KHTML, like Gecko) Chrome/20.0.1092.0 Safari/536.6"
  ]

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
  -- , form :: [Wreq.FormParam]
  , form :: Params
  -- , payloadDefault :: [Wreq.FormParam]
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

mkHeader :: FBSession -> Header
mkHeader fbSession = Header
  { contentType = "application/x-www-form-urlencoded"
  , referer = Encoding.encodeUtf8 baseURL
  , origin = Encoding.encodeUtf8 baseURL
  , headerUserAgent = sessionUserAgent fbSession
  , connection = "keep-alive"
  }

defaultHeader :: Header
defaultHeader = Header
  { contentType = "application/x-www-form-urlencoded"
  , referer = Encoding.encodeUtf8 baseURL
  , origin = Encoding.encodeUtf8 baseURL
  , headerUserAgent = Encoding.encodeUtf8 (head userAgents)
  , connection = "keep-alive"
  }

headerOptions :: Header -> Wreq.Options -> Wreq.Options
headerOptions h = foldr1 (.) updates
  where
    updates :: [Wreq.Options -> Wreq.Options]
    updates = [ -- Wreq.header "Content-Type" .~ [contentType h]
                Wreq.header "Referer"      .~ [referer h]
              , Wreq.header "Origin"       .~ [origin h]
              , Wreq.header "User-Agent"   .~ [headerUserAgent h]
              , Wreq.header "Connection"   .~ [connection h]
              ]

doHeader = headerOptions . mkHeader

type Params = [(Text, Text)]

post' :: String -> Params -> StateT FBSession IO (Wreq.Response LByteString)
post' url params = do
  fbState <- State.get
  payload <- generatePayload params
  State.liftIO $ Wreq.Session.post (session fbState) url (paramsToFormParams payload)

  where
    paramsToFormParams :: Params -> [FormParam]
    paramsToFormParams ps = [ (encode name) := (encode value) | (name, value) <- ps ]

get' :: String -> Params -> StateT FBSession IO (Wreq.Response LByteString)
get' url params = do
  fbState <- State.get
  payload <- generatePayload params
  let opts = Wreq.defaults & foldr1 (.) (map toParam payload)
  State.liftIO $ Wreq.Session.getWith opts (session fbState) url
  where
    toParam (name, value) = Wreq.param name .~ [value]

generatePayload :: Monad m => Params -> StateT FBSession m Params
generatePayload params = do
  fbState <- State.get
  let count' = (requestCounter fbState) + 1

      encodeDigit x
        | 0  <= x && x < 10 = Char.chr (x - Char.ord '0')
        | 10 <= x && x < 36 = Char.chr ((x - 10) - Char.ord 'a')

      params' = (payloadDefault fbState) ++ params
                ++ [("__req", Text.pack $ Numeric.showIntAtBase 36 encodeDigit count' "")]

  State.put $ fbState { requestCounter = count' }
  return params

login :: Wreq.Session.Session -> Authentication -> IO FBSession
login session (Authentication username password) = do
  loginPage <- Wreq.Session.get session mobileURL
  -- print (loginPage ^. Wreq.responseBody)

  let
    doc = XML.fromDocument (HTML.parseLBS (loginPage ^. Wreq.responseBody))
    inputs = (doc XML.$.// XML.attributeIs "id" "login_form") >>= (XML.$.// XML.element "input")
    formUrl = (doc XML.$.// XML.attributeIs "id" "login_form" >=> XML.attribute "action")

    pairs :: [FormParam]
    pairs = [   (Encoding.encodeUtf8 $ concat (XML.attribute "name"  cursor))
             := (Encoding.encodeUtf8 $ concat (XML.attribute "value" cursor))
            | cursor <- inputs ]

    additionalPairs = [ "email" := username
                      , "pass"  := password
                      , "login" := ("Log In" :: ByteString)
                      ]

    -- additionalPairs entries take precedence over pairs entries
    -- payload = Aeson.toJSON (HashMap.fromList (pairs ++ additionalPairs))
    payload = pairs ++ additionalPairs

    opts = Wreq.defaults & headerOptions defaultHeader

  case loginPage ^? Wreq.responseBody of
    Just html -> writeFile "/home/sidharth/login_page.html" (fromShow html)
    _ -> return ()

  -- loginResponse <- Wreq.postWith opts loginURL payload
  loginResponse <- Wreq.Session.postWith opts session (textToString $ concat formUrl) payload

  -- print (loginResponse ^? Wreq.responseBody)
  -- print (payload)
  -- print (formUrl)

  clientId <- Text.pack . flip Numeric.showHex "" <$> (Random.randomIO :: IO Int)
  now <- Time.getCurrentTime
  let
    startTime = round ((Time.POSIX.utcTimeToPOSIXSeconds now) * 1000)

    uid :: Maybe Integer
    uid = (loginResponse ^? Wreq.responseCookie "c_user" . Wreq.cookieValue) >>= (readMay . Encoding.decodeUtf8)
    userChannel = (\s -> "p_" <> show s) <$> uid

    doc2 = XML.fromDocument (HTML.parseLBS (loginResponse ^. Wreq.responseBody))
    fb_dtsg = concat $ doc2 XML.$.// (XML.attributeIs "name" "fb_dtsg" >=> XML.attribute "value")

    mkTTStamp :: Text -> Text
    mkTTStamp s = concat [show (Char.ord c) <> "2" | c <- s ^.. Text.unpacked . traverse ]

    ttStamp = mkTTStamp fb_dtsg

    revision :: Text
    revision = "2246636"

  print uid
  print ttStamp
  print (loginResponse ^? Wreq.responseCookie "c_user")

  case loginResponse ^? Wreq.responseBody of
    Just html -> writeFile "/home/sidharth/fb.html" (fromShow html)
    _ -> return ()

  let
    payloadDefault :: Params
    payloadDefault =
      [ ("__rev",   revision)
      , ("__user",  show uid)
      , ("__a",     "1")
      , ("ttstamp", ttStamp)
      , ("fb_dtsg", fb_dtsg)
      ]

    form :: Params
    form =
      [ ("channel"    , Maybe.fromJust userChannel)
      , ("partition"  , "-2")
      , ("clientid"   , clientId)
      , ("viewer_uid" , show uid)
      , ("uid"        , show uid)
      , ("state"      , "active")
      , ("format"     , "json")
      , ("idle"       , "0")
      , ("cap"        , "8")
      ]

    prev     = now
    tmpPrev  = now
    lastSync = now

    -- todo throw/return an error if there is a failure here
    fbSession = FBSession
      { session = session
      , requestCounter = 1
      , seq' = ""
      , sessionUserAgent = Encoding.encodeUtf8 (head userAgents)
      , prev = prev
      , tmpPrev = tmpPrev
      , lastSync = lastSync
      , form = form
      , payloadDefault = payloadDefault
      , ttStamp = ttStamp
      , uid = Maybe.fromJust uid
      , userChannel = Maybe.fromJust userChannel
      , clientId = clientId
      }

  -- TODO Somehow check if login was successful and throw/return the error?
  return fbSession


-- defaultPayload :: FBSession -> Payload
-- defaultPayload fbSession = Payload
--   {
--   user =
--   }
