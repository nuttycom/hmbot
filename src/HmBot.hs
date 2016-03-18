{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module HmBot 
  --(runApp, app) 
  where

import           Control.Monad.IO.Class
import           Data.Aeson (Value(..), object, (.=))
import qualified Data.Map.Lazy as M
import           Data.Text
import           Text.Parsec (many, manyTill, skipMany)
import           Text.Parsec.Text
import           Text.Parsec.Char (char, noneOf, anyChar)

import           Network.Wai (Application)
import qualified Web.Scotty as S

runApp :: IO ()
runApp = S.scotty 8080 app'

app :: IO Application
app = S.scottyApp app'

app' :: S.ScottyM ()
app' = do
  S.post "/" $ do
    request <- readPost
    response <- liftIO $ handlePost request
    S.json $ M.singleton ("text" :: Text) response
    
-- Request form data:
--token=XXXXXXXXXXXXXXXXXX
--team_id=T0001
--team_domain=example
--channel_id=C2147483705
--channel_name=test
--timestamp=1355517523.000005
--user_id=U2147483697
--user_name=Steve
--text=googlebot: What is the air-speed velocity of an unladen swallow?
--trigger_word=googlebot:
data SlackPost = SlackPost
  { token :: Text
  , teamId :: Text
  , teamDomain :: Text
  , channelId :: Text
  , userId :: Text
  , userName :: Text
  , text :: Text
  , triggerWord :: Text
  }
     
readPost :: S.ActionM SlackPost
readPost = 
  SlackPost <$> S.param "token"
            <*> S.param "team_id"
            <*> S.param "team_domain"
            <*> S.param "channel_id"
            <*> S.param "user_id"
            <*> S.param "user_name"
            <*> S.param "text"
            <*> S.param "trigger_word"

handlePost :: SlackPost -> IO Text
handlePost p = pure "hi"

allBracketed :: Parser [Text]
allBracketed = many (ignored *> bracketed <* ignored)

ignored :: Parser ()
ignored = skipMany (noneOf "[")

bracketed :: Parser Text
bracketed = pack <$> (char '[' >> manyTill anyChar (char ']'))

