{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import BasicPrelude

import Lib

import qualified Network.Wreq.Session as Wreq.Session
import qualified Control.Monad.State as State

main :: IO ()
main = Wreq.Session.withSession $ \session -> do
  [username, password] <- lines <$> readFile "credentials"
  let auth = Authentication username password
  fbSessionMaybe <- login session auth
  case fbSessionMaybe of
    Just fbSession -> void $ State.runStateT getThreadList fbSession
    Nothing -> putStrLn "error logging in"
