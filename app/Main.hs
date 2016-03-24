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
  fbSession <- login session auth
  void $ State.runStateT getThreadList fbSession
