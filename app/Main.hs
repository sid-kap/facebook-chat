{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import BasicPrelude

import Lib

import qualified Network.Wreq.Session as Wreq.Session

main :: IO ()
main = Wreq.Session.withSession $ \session -> do
  [username, password] <- lines <$> readFile "credentials"
  let auth = Authentication username password
  void $ login session auth
