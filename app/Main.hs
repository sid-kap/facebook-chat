{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import BasicPrelude

import Web.FacebookChat.Lib
import Web.FacebookChat.Types
import Web.FacebookChat.Stickers
import Control.Concurrent as Concurrent

import           Control.Lens         ((&), (.~), (^.), (^@..), (^?!), over, (%~))
import qualified Network.Wreq.Session as Wreq.Session
import qualified Network.Wreq         as Wreq
import qualified Control.Monad.State  as State

import qualified Data.Colour.RGBSpace.HSL as Colour (hsl)
import qualified Data.Colour.SRGB         as Colour (sRGB24show, sRGB)
import qualified Data.Colour.RGBSpace     as Colour (uncurryRGB)

main :: IO ()
main = do
  [username, password] <- lines <$> readFile "credentials"
  let auth = Authentication username password
  fbSessionMaybe <- login auth
  case fbSessionMaybe of
    Just fbSession -> do
      putStrLn "successfully logged in"
      void $ State.runStateT action fbSession
    Nothing -> putStrLn "error logging in"

action = do
  -- sendMessage undefined (Message "hello" (Just (Files ["stuff"])))
  -- res <- makeAttachmentParams (Files ["stuff", "more_stuff"])
  [group] <- searchForThread "centimeme"
  print group
  -- setTitle (thread_fbid group) "get muted'"
  -- markAsRead (thread_fbid group)
  -- getThreadList 0 20 >>= print
  -- chris <- getUserId "chris denny"
  -- print chris
  -- res <- makeAttachmentParams (URL "http://google.com")
  -- print group
  -- print res
  -- sendMessage (ToGroup (thread_fbid group))
  --   (Message "here is a sticker" (Just (Sticker mThumb)))
    -- (Message "here is 2 files" (Just (Files ["stuff", "more_stuff"])))
    -- (Message "here's a link" (Just (URL "http://google.com")))
  -- forM_ [1..40] $ \i -> do
  --   -- sendMessage (ToGroup (thread_fbid group)) (Message ("testing " <> show i) Nothing)
  --   sendMessage (ToGroup (thread_fbid group)) (Message "sup ashwin" Nothing)
  --   liftIO $ Concurrent.threadDelay 500000 -- 1000000

  -- sendMessage (ToGroup (thread_fbid group)) (Message "testing " Nothing)

    -- (Message "testing" (Just (URL "http://google.com")))
    -- (Message "testing" (Just (Files ["stuff"])))
  -- let
  --   threadId = ResponseTypes.thread_id attGroup
  --   colors = [ Colour.hsl hue 1 0.3 | hue <- [0,20..360] ]

  -- putStrLn (ResponseTypes.name attGroup)

  -- [thread] <- searchForThread "food group"
  -- let colorString = "#887663"
  -- let colorString = "#ffffff"
  -- let colorString = "#888888"
  -- setThreadColor colorString (ResponseTypes.thread_id thread)

  -- forM_ colors $ \c -> do
  --   liftIO $ print c
  --   let colorString =
  --         fromString (Colour.sRGB24show (Colour.uncurryRGB Colour.sRGB c))
  --   print colorString
  --   setThreadColor colorString threadId
  --   liftIO $ Concurrent.threadDelay 3000000
