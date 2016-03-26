{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import BasicPrelude

import Lib

import Lens.Family ((&), (.~), (^.), (^..), (^?), over)
import qualified Network.Wreq.Session as Wreq.Session
import qualified Network.Wreq as Wreq
import qualified Control.Monad.State as State

main :: IO ()
main = Wreq.Session.withSession $ \session -> do
  [username, password] <- lines <$> readFile "credentials"
  let auth = Authentication username password
  fbSessionMaybe <- login session auth
  case fbSessionMaybe of
    Just fbSession -> void $ State.runStateT action fbSession
    Nothing -> putStrLn "error logging in"

action = do
  getThreadList 0 20
  -- uid <- getUserId "srishti"
  -- liftIO $ print (uid ^. Wreq.responseBody)
  -- liftIO (getFriendsList >>= friends)
  -- getOnlineUsers >>= (liftIO . print)
  searchForThread "food" >>= (liftIO . print)
  return ()
