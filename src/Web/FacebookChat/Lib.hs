{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.FacebookChat.Lib
    where

import BasicPrelude
import qualified Control.Applicative as Applicative
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Foldable
import qualified Control.Exception.Base as Exception

import qualified Control.Monad.State as State
import Control.Monad.State (State, StateT, MonadState)
import qualified Control.Monad.Catch as Catch

import qualified Data.Text.Encoding as Encoding
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified System.Random as Random

import qualified Network.Wreq as Wreq
import Network.Wreq (FormParam((:=)))
import qualified Network.HTTP.Client as HTTP

import Control.Lens ((&), (.~), (^.), (^..), (^@..), (^?!), over, _Left, _head, _1, _2, (%~))
import qualified Control.Lens as Lens ((^?), elementsOf)

import qualified Numeric
import qualified Data.Time.Clock as Time
import qualified Data.Time.LocalTime as Time
import qualified Data.Time.Clock.POSIX as Time.POSIX

import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson                (FromJSON)
import qualified Data.Aeson.Lens  as Aeson

import qualified Data.Scientific as Scientific

import qualified Text.HTML.DOM as HTML
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML

import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lens as ByteString
import qualified Data.ByteString      as ByteString
import qualified Data.Text.Lens as Text

import Web.FacebookChat.Types

import Debug.Trace

newtype FBException = FBException Text deriving Show
instance Exception FBException

-----------------------------------------------------------
-- helper functions
-----------------------------------------------------------

x ^? lens = maybeToError (x Lens.^? lens)
infixl 8 ^?

(%:=) :: ByteString -> Text -> FormParam
(%:=) = (:=)

parseJson :: Catch.MonadThrow m => LByteString -> m Aeson.Value
parseJson str =
  if first == prefix
    then decode rest
    else Catch.throwM (FBException ("expected" <> show prefix <> ", but found " <> show rest))
  where
    prefix = "for (;;);"
    (first, rest) = ByteString.Lazy.splitAt (ByteString.Lazy.length prefix) str

encode = Encoding.encodeUtf8

eitherToError :: Catch.MonadThrow m => Either String b -> m b
eitherToError = either (Catch.throwM . FBException . fromString) return

maybeToError :: Catch.MonadThrow m => Maybe b -> m b
maybeToError = maybe (Catch.throwM (FBException "was Maybe")) return

decode :: (FromJSON a, Catch.MonadThrow m) => LByteString -> m a
decode = eitherToError . Aeson.eitherDecode

parse :: (FromJSON a, Catch.MonadThrow m) => (a -> Aeson.Parser b) -> a -> m b
parse f = eitherToError . (Aeson.parseEither f)

parseValue :: (FromJSON a, Catch.MonadThrow m) => Aeson.Value -> m a
parseValue = parse Aeson.parseJSON

timeToMilliseconds :: Integral a => Time.UTCTime -> a
timeToMilliseconds time = round ((Time.POSIX.utcTimeToPOSIXSeconds time) * 1000)

responseToXML :: Wreq.Response LByteString -> XML.Cursor
responseToXML response = (XML.fromDocument . HTML.parseLBS) (response ^. Wreq.responseBody)

-----------------------------------------------------------
-- API helper functions
-----------------------------------------------------------

data Authentication = Authentication
  { username :: Text
  , password :: Text
  }

data FBSession = FBSession
  { cookieJar :: HTTP.CookieJar
  , requestCounter :: Int
  , payloadDefault :: [(Text, Text)]
  , uid :: Integer
  }

data Message = Message Text (Maybe Attachment)
data Attachment = Sticker Text | Files [FilePath] | URL Text
data Recipient = NewGroup [UserId] | ToUser UserId | ToGroup Text

sessionOpts :: FBSession -> Wreq.Options
sessionOpts session =
  Wreq.defaults & Wreq.cookies .~ (Just (cookieJar session))

postParts :: String -> [Wreq.Part] -> StateT FBSession IO (Wreq.Response LByteString)
postParts url parts = do
  opts <- sessionOpts <$> State.get
  payload <- generatePayload
  liftIO $ Wreq.postWith opts url (parts ++ map paramToPart payload)

  where
    paramToPart :: (Text, Text) -> Wreq.Part
    paramToPart (name, value) = Wreq.partText name value

post' :: String -> [FormParam] -> StateT FBSession IO (Wreq.Response LByteString)
post' url params = do
  opts <- sessionOpts <$> State.get
  payload <- generatePayload
  liftIO $ Wreq.postWith opts url ((map paramToFormParam payload) ++ params)

  where
    paramToFormParam :: (Text, Text) -> FormParam
    paramToFormParam (name, value) = (encode name) := (encode value)

get' :: String -> [(Text, Text)] -> StateT FBSession IO (Wreq.Response LByteString)
get' url params = do
  opts <- sessionOpts <$> State.get
  payload <- generatePayload
  let opts' = opts & foldr1 (.) (map toParam (payload ++ params))
  liftIO $ Wreq.getWith opts' url
  where
    toParam (name, value) = Wreq.param name .~ [value]

-- Not sure that this function should have side effects (incrementing the
-- request counter). Maybe this should be done in get' and post'.
generatePayload :: MonadState FBSession m => m [(Text, Text)]
generatePayload = do
  fbState <- State.get
  let
    count' = (requestCounter fbState) + 1

    encodeDigit x
      | 0  <= x && x < 10 = Char.chr (x + Char.ord '0')
      | 10 <= x && x < 36 = Char.chr ((x - 10) + Char.ord 'a')

    params' = (payloadDefault fbState)
              ++ [("__req", Text.pack $ Numeric.showIntAtBase 36 encodeDigit count' "")]

  State.put $ fbState { requestCounter = count' }
  return params'

-----------------------------------------------------------
-- API functions
-----------------------------------------------------------

getUserId :: Text -> StateT FBSession IO Aeson.Value
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
    >>= (^? Wreq.responseBody)
    >>= parseJson

-- TODO accept start/end arguments
getThreadList :: Int -> Int -> StateT FBSession IO [Thread]
getThreadList start end = do
  let
    form =
      [ "client"        := ("mercury" :: Text)
      , "inbox[offset]" := start
      , "inbox[limit]"  := (end - start)
      ]

  post' "https://www.facebook.com/ajax/mercury/threadlist_info.php" form
    >>= (^? Wreq.responseBody)
    >>= parseJson
    >>= (^? Aeson.key "payload" . Aeson.key "threads")
    >>= parseValue

getFriendsList :: StateT FBSession IO (HashMap Text Friend)
getFriendsList = do
  uid_ <- uid <$> State.get
  let form = ["viewer" := (show uid_)]

  post' "https://www.facebook.com/chat/user_info_all" form
    >>= (^? Wreq.responseBody)
    >>= parseJson
    >>= (^? Aeson.key "payload")
    >>= parseValue

getUserInfo :: [UserId] -> StateT FBSession IO (HashMap UserId Friend)
getUserInfo userIds = do
  let form = [ ("ids[" <> (encode $ show i) <> "]") := v | (i,v) <- zip [1..] userIds ]

  post' "https://www.facebook.com/chat/user_info/" form
    >>= (^? Wreq.responseBody)
    >>= parseJson
    >>= (^? Aeson.key "payload" . Aeson.key "profiles")
    >>= parseValue

getOnlineUsers :: StateT FBSession IO (HashMap UserId Status, HashMap UserId Integer)
getOnlineUsers = do
  uid_ <- uid <$> State.get
  -- TODO specify this using Bool type instead of strings
  let form = [ "user" := (show uid_)
             , "fetch_mobile" := ("false" :: Text)
             , "get_now_available_list" := ("true" :: Text)
             ]
  buddyList <- post' "https://www.facebook.com/ajax/chat/buddy_list.php" form
              >>= (^? Wreq.responseBody)
              >>= parseJson
              >>= (^? Aeson.key "payload" . Aeson.key "buddy_list")

  nowAvailableList <- buddyList ^? Aeson.key "nowAvailableList"
                      >>= parseValue . over Aeson.members (^?! Aeson.key "a")

  lastActiveTimes <- buddyList ^? Aeson.key "last_active_times"
                      >>= parseValue

  return (nowAvailableList, lastActiveTimes)

searchForThread :: Text -> StateT FBSession IO [Thread]
searchForThread query = do
  let form = [ "client" := ("web_messenger" :: Text)
             , "query"  := query
             , "offset" := (0 :: Int)
             , "limit"  := (21 :: Int)
             , "index"  := ("fbid" :: Text)
             ]
  post' "https://www.facebook.com/ajax/mercury/search_threads.php" form
    >>= (^? Wreq.responseBody)
    >>= parseJson
    >>= (^? Aeson.key "payload" . Aeson.key "mercury_payload" . Aeson.key "threads")
    >>= parseValue

-- start is True to start typing, False to stop typing
sendTypingIndicator :: ThreadId -> Bool -> StateT FBSession IO ()
sendTypingIndicator threadId start = do
  let isUser = False -- TODO check isUser, or rely on user to send this
      form = [ "typ"    := ((if start then 1 else 0) :: Int) -- 1 to start typing, 0 to end typing
             , "to"     := (if isUser then threadId else "")
             , "source" := ("mercury-chat" :: Text)
             , "thread" := threadId
             ]
  void (post' "https://www.facebook.com/ajax/messaging/typ.php" form)

removeUserFromGroup :: ThreadId -> StateT FBSession IO ()
removeUserFromGroup threadId = do
  uid_ <- uid <$> State.get
  let form = [ "uid" := uid_
             , "tid" := threadId
             ]
  void (post' "https://www.facebook.com/chat/remove_participants" form)

setThreadColor :: Text -> ThreadId -> StateT FBSession IO ()
setThreadColor color threadID = do
  let form = [ "color_choice"         := color
             , "thread_or_other_fbid" := threadID
             ]
  response <- post' "https://www.messenger.com/messaging/save_thread_color/?source=thread_settings&dpr=1" form
              >>= (^? Wreq.responseBody)
              >>= parseJson

  case response ^? Aeson.key "errorSummary" of
    Just (Aeson.String err) -> throwIO (FBException err)
    Nothing -> return ()

formEncode :: ByteString -> Aeson.Value -> [FormParam]
formEncode s value = case value of
  Aeson.Object o   -> doIndices (HashMap.toList o)
  Aeson.Array arr  -> doIndices (zip (map show [0..]) (Foldable.toList arr))
  Aeson.Number n   -> [s := numStr]
    where numStr = case Scientific.floatingOrInteger n of
            Left floating -> show floating
            Right integer -> show integer
  Aeson.String str -> [s := str]
  Aeson.Bool bool  -> [s := boolStr]
    where boolStr :: Text = if bool then "true" else "false"
  Aeson.Null       -> [s := ("null" :: Text)]
  where
    doIndices :: [(Text, Aeson.Value)] -> [FormParam]
    doIndices pairs = concat $
      map (\(i, elem) -> formEncode (s <> "[" <> encode i <> "]") elem) pairs

makeAttachmentParams :: Attachment -> StateT FBSession IO [FormParam]
makeAttachmentParams (Files files) = do
  metadatas :: [[(Text, Aeson.Value)]] <- forM files $ \filepath -> do
    json <- postParts "https://upload.facebook.com/ajax/mercury/upload.php" [Wreq.partFileSource "upload_1024" filepath]
            >>= (^? Wreq.responseBody)
            >>= parseJson

    case json ^? Aeson.key "errorSummary" of
      Just err -> throwIO (FBException (show err))
      Nothing  -> return (json ^@.. Aeson.key "payload" . Aeson.key "metadata" . Aeson.nth 0 . Aeson.members)

  let
    attachmentTypes = ["image_id", "gif_id", "file_id", "video_id"]

    mkParam :: (Text, Aeson.Value) -> Maybe (Text, Text)
    mkParam (t, Aeson.String aid) | t `elem` attachmentTypes = Just (t, aid)
    mkParam _ = Nothing

    allAttachments :: [(Text, Text)]
    allAttachments = Maybe.mapMaybe mkParam (concat metadatas)

    attachmentsForm =
      [ ("message_batch[0][" <> encode attachmentType <> "s][" <> encode (show i) <> "]") := value
      | (i, (attachmentType, value)) <- zip [1..] allAttachments ]

  return attachmentsForm

makeAttachmentParams (URL url) = do
  let form = [ "image_height" := ("960" :: Text)
             , "image_width"  := ("960" :: Text)
             , "uri"          := url]
  json <- post' "https://www.facebook.com/message_share_attachment/fromURI/" form
           >>= (^? Wreq.responseBody)
           >>= parseJson

  case json ^? Aeson.key "payload" . Aeson.key "share_data" . Aeson.key "share_params" of
    Just share_params -> do
      return (
       ["message_batch[0][shareable_attachment][share_type]" := ("100" :: Text)]
       ++ (formEncode "message_batch[0][shareable_attachment][share_params]" share_params)
       )
    Nothing -> throwIO (FBException "Invalid url")

makeAttachmentParams (Sticker stickerId) =
  return [ "message_batch[0][sticker_id]" := stickerId ]

sendMessage :: Recipient -> Message -> StateT FBSession IO ()
sendMessage recipient (Message text attachment) = do
  attachmentParams <- case attachment of
    Just attachment' -> makeAttachmentParams attachment'
    Nothing -> return []

  uid_ <- uid <$> State.get
  now <- liftIO Time.getCurrentTime
  timeOfDay <- liftIO
                (Time.localTimeOfDay . Time.zonedTimeToLocalTime <$>
                  Time.utcToLocalZonedTime now)

  signatureId <- liftIO (show <$> (Random.randomIO :: IO Int))
  messageAndOTID <- liftIO (show <$> (Random.randomIO :: IO Int))
  let
    currentTimeMillis = timeToMilliseconds now

    formShared =
      [ "client" %:= "mercury"
      , "message_batch[0][action_type]" %:= "ma-type:user-generated-message"
      , "message_batch[0][author]" := ("fbid:" <> (show uid_))
      , "message_batch[0][timestamp]" := show currentTimeMillis
      , "message_batch[0][timestamp_absolute]" %:= "Today"
      , "message_batch[0][timestamp_relative]" %:=
          (show (Time.todHour timeOfDay) <> ":" <> show (Time.todMin timeOfDay))
      , "message_batch[0][timestamp_time_passed]" %:= "0"
      , "message_batch[0][is_unread]" %:= "false"
      , "message_batch[0][is_cleared]" %:= "false"
      , "message_batch[0][is_forward]" %:= "false"
      , "message_batch[0][is_filtered_content]" %:= "false"
      , "message_batch[0][is_filtered_content_bh]" %:= "false"
      , "message_batch[0][is_filtered_content_account]" %:= "false"
      , "message_batch[0][is_filtered_content_quasar]" %:= "false"
      , "message_batch[0][is_filtered_content_invalid_app]" %:= "false"
      , "message_batch[0][is_spoof_warning]" %:= "false"
      , "message_batch[0][source]" %:= "source:chat:web"
      , "message_batch[0][source_tags][0]" %:= "source:chat"
      , "message_batch[0][body]" %:= text
      , "message_batch[0][html_body]" %:= "false"
      , "message_batch[0][ui_push_phase]" %:= "V3"
      , "message_batch[0][status]" %:= "0"
      , "message_batch[0][offline_threading_id]" := messageAndOTID
      , "message_batch[0][message_id]" := messageAndOTID
      , "message_batch[0][threading_id]" %:= "<1459393423561:1120090019-foo@mail.projektitan.com>"
      , "message_batch[0][ephemeral_ttl_mode]" %:= "0"
      , "message_batch[0][manual_retry_cnt]" %:= "0"
      , "message_batch[0][has_attachment]" := case Maybe.isJust attachment of
        True -> "true"
        False -> "false" :: Text
      , "message_batch[0][signatureID]" %:= signatureId -- "139284019"
      ]

    form = case recipient of
      NewGroup userIds ->
        [ ("message_batch[0][specific_to_list][" <> (encode $ show i) <> "]") := ("fbid:" <> userId)
        | (i, userId) <- zip [0..] (userIds ++ [show uid_]) ]
        ++ [ "message_batch[0][client_thread_id]" := ("root:" <> messageAndOTID :: Text)]
      ToUser userId ->
        [ "message_batch[0][specific_to_list][0]" := ("fbid:" <> userId)
        , "message_batch[0][specific_to_list][1]" := ("fbid:" <> show uid_)
        , "message_batch[0][other_user_fbid]"     := userId
        ]
      ToGroup threadId ->
        [ "message_batch[0][thread_fbid]" := threadId ]

  void (post' "https://www.facebook.com/ajax/mercury/send_messages.php" (attachmentParams ++ formShared ++ form))

markAsRead :: ThreadId -> StateT FBSession IO ()
markAsRead threadId =
  void (post' "https://www.facebook.com/ajax/mercury/change_read_status.php" form)
  where form = ["ids[" <> encode threadId <> "]" := ("true" :: Text)]

setTitle :: ThreadId -> Text -> StateT FBSession IO ()
setTitle threadId title = do
  uid_ <- uid <$> State.get
  messageAndOTID <- liftIO (Random.randomIO :: IO Int)
  currentTimeMillis <- liftIO (timeToMilliseconds <$> Time.getCurrentTime)
  let form =
       [ "client" %:= "mercury"
       , "message_batch[0][action_type]" %:= "ma-type:log-message"
       , "message_batch[0][author]" %:= ("fbid:" <> (show uid_))
       , "message_batch[0][thread_id]" %:= ""
       , "message_batch[0][author_email]" %:= ""
       , "message_batch[0][coordinates]" %:= ""
       , "message_batch[0][timestamp]" := (currentTimeMillis :: Int)
       , "message_batch[0][timestamp_absolute]" %:= "Today"
       , "message_batch[0][timestamp_relative]" %:= "12:00"
       , "message_batch[0][timestamp_time_passed]" %:= "0"
       , "message_batch[0][is_unread]" %:= "false"
       , "message_batch[0][is_cleared]" %:= "false"
       , "message_batch[0][is_forward]" %:= "false"
       , "message_batch[0][is_filtered_content]" %:= "false"
       , "message_batch[0][is_spoof_warning]" %:= "false"
       , "message_batch[0][source]" %:= "source:chat:web"
       , "message_batch[0][source_tags][0]" %:= "source:chat"
       , "message_batch[0][status]" %:= "0"
       , "message_batch[0][offline_threading_id]" := messageAndOTID
       , "message_batch[0][message_id]" := messageAndOTID
       , "message_batch[0][threading_id]"%:= "<1459393423561:1120090019-foo@mail.projektitan.com>"
       , "message_batch[0][manual_retry_cnt]" %:= "0"
       , "message_batch[0][thread_fbid]" %:= threadId
       , "message_batch[0][log_message_data][name]" %:= title
       , "message_batch[0][log_message_type]" %:= "log:thread-name"
       ]

  void (post' "https://www.facebook.com/ajax/mercury/send_messages.php" form)

login :: Authentication -> IO (Maybe FBSession)
login (Authentication username password) = do
  loginPage <- Wreq.get "https://m.facebook.com/"

  let
    doc = responseToXML loginPage
    inputs = (doc XML.$.// XML.attributeIs "id" "login_form")
              >>= (XML.$.// XML.element "input")
    formUrl = textToString $ concat $ doc XML.$.// XML.attributeIs "id" "login_form" >=> XML.attribute "action"

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

  loginResponse <- Wreq.post formUrl payload

  let
    uidMaybe :: Maybe Integer
    uidMaybe = (loginResponse ^? Wreq.responseCookie "c_user" . Wreq.cookieValue) >>= (readMay . Encoding.decodeUtf8)

  case uidMaybe of
    Nothing -> return Nothing

    Just uid -> do
      let
        doc' = responseToXML loginResponse

        fb_dtsg = concat $ doc' XML.$.// (XML.attributeIs "name" "fb_dtsg" >=> XML.attribute "value")

        ttStamp :: Text
        ttStamp = concat [show (Char.ord c) <> "2" | c <- Text.unpack fb_dtsg]

        payloadDefault :: [(Text, Text)]
        payloadDefault =
          [ ("__rev",   "2246636")
          , ("__user",  show uid)
          , ("__a",     "1")
          , ("ttstamp", ttStamp)
          , ("fb_dtsg", fb_dtsg)
          ]

        cookies :: [HTTP.Cookie]
        cookies = HTTP.destroyCookieJar (loginResponse ^. Wreq.responseCookieJar)

        messengerCookies  :: [HTTP.Cookie]
        messengerCookies =
          map (\c -> c { HTTP.cookie_domain = "messenger.com" })
          $ filter (\c -> HTTP.cookie_domain c == "facebook.com") cookies

        newCookieJar :: HTTP.CookieJar
        newCookieJar = HTTP.createCookieJar (cookies ++ messengerCookies)

        fbSession = FBSession
          { cookieJar = newCookieJar
          , requestCounter = 1
          , payloadDefault = payloadDefault
          , uid = uid
          }

      return (Just fbSession)
