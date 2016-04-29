{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module HmBot.HmBot 
  --(runApp, app) 
  where

import           Control.Applicative
import           Control.Error.Util
import           Control.Lens ((&), (.~), (^.))
import           Control.Monad.Except
import           Control.Monad.IO.Class

import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..), (.:), (.=), object) 
import           Data.Aeson.Lens
import           Data.Either.Combinators (mapLeft)
import           Data.Maybe (fromMaybe)
import           Data.Semigroup ((<>))
import qualified Data.Map.Lazy as M
import           Data.Text

import           Network.Wai (Application)

import           Web.Slack
import           Web.Slack.Message

import           System.Environment (lookupEnv)

import           HmBot.MTGAPI

hmcfg :: String -> SlackConfig
hmcfg apiToken = SlackConfig
  { _slackApiToken = apiToken 
  }

run :: IO ()
run = do
  apiToken <- fromMaybe (error "SLACK_API_TOKEN not set") <$> lookupEnv "SLACK_API_TOKEN"
  runBot (hmcfg apiToken) hmbot ()

hmbot :: SlackBot ()
hmbot (Message cid _ msg _ _ _) = sendMessage cid msg
hmbot _ = pure ()
