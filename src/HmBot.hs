{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module HmBot 
  --(runApp, app) 
  where

import           Control.Error.Util
import           Control.Lens ((&), (.~), (^.))
import           Control.Monad.Except
import           Control.Monad.IO.Class

import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..), (.:), (.=), object) 
import           Data.Aeson.Lens
import           Data.Either.Combinators (mapLeft)
import           Data.Semigroup ((<>))
import qualified Data.Map.Lazy as M
import           Data.Text

import           Text.Parsec (many, manyTill, parse, skipMany)
import           Text.Parsec.Error
import           Text.Parsec.Text
import           Text.Parsec.Char (char, noneOf, anyChar)

import           Network.Wai (Application)
import           Network.Wreq
import qualified Web.Scotty as S

runApp :: IO ()
runApp = S.scotty 8080 app'

app :: IO Application
app = S.scottyApp app'

app' :: S.ScottyM ()
app' = do
  S.post "/" $ do
    request <- readPost
    response <- liftIO . runExceptT $ handlePost request
    S.json $ either reportError id response

reportError :: HmError -> SlackResponse
reportError (ParseFailure messages) = 
  let errText = "Errors encountered parsing request: " <> intercalate ";" ((pack . messageString) <$> messages)
  in  SlackResponse "mtg" True errText []

reportError (NotFound xs) = 
  let errText = "Cards not found: " <> (intercalate ", " xs)
  in  SlackResponse "mtg" True errText []
    
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
data SlackRequest = SlackRequest
  { token :: Text
  , teamId :: Text
  , teamDomain :: Text
  , channelId :: Text
  , userId :: Text
  , userName :: Text
  , text :: Text
  , triggerWord :: Text
  }

data HmError 
  = ParseFailure [Message]
  | NotFound [Text]
     
data SlackAttachment = SlackAttachment 
  { title :: Text
  , title_link :: Text
  , image_url :: Text
  }

instance ToJSON SlackAttachment where
  toJSON (SlackAttachment t l u) = 
    object [ "title" .= t
           , "title_link" .= l
           , "image_url" .= u
           ]

data SlackResponse = SlackResponse
  { channel :: Text
  , as_user :: Bool
  , message :: Text
  , attachments :: [SlackAttachment]
  }

instance ToJSON SlackResponse where
  toJSON (SlackResponse c _ m a) = 
    object [ "channel" .= c
           , "as_user" .= True
           , "text" .= m
           , "attachments" .= a
           ]

readPost :: S.ActionM SlackRequest
readPost = 
  SlackRequest <$> S.param "token"
            <*> S.param "team_id"
            <*> S.param "team_domain"
            <*> S.param "channel_id"
            <*> S.param "user_id"
            <*> S.param "user_name"
            <*> S.param "text"
            <*> S.param "trigger_word"

handlePost :: SlackRequest -> ExceptT HmError IO SlackResponse
handlePost p = do
  cardNames <- hoistEither . mapLeft (ParseFailure . errorMessages) $ commands p
  cards  <- lift $ traverse lookupCard cardNames
  pure $ slackResponse "mtg" cards

slackResponse :: Text -> [Card] -> SlackResponse
slackResponse channel cards = SlackResponse channel True "" (cardAttachment <$> cards)

cardAttachment :: Card -> SlackAttachment
cardAttachment c = 
  SlackAttachment 
    (name c) 
    ("http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=" <> multiverseId c)
    (gathererImage c)


commands :: SlackRequest -> Either ParseError [Text]
commands req = 
  let input = text req
  in  parse allBracketed (unpack input) input

allBracketed :: Parser [Text]
allBracketed = many (ignored *> bracketed <* ignored)

ignored :: Parser ()
ignored = skipMany (noneOf "[")

bracketed :: Parser Text
bracketed = pack <$> (char '[' >> manyTill anyChar (char ']'))

-- var attachments = 
--   JSON.stringify([{
--   title: bestMatch.doc.name,
--   title_link: 'http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid='+card.multiverseid,
--   image_url: card.images.gatherer
--   }]);

--   slack.api("chat.postMessage", {channel: data.channel, as_user: true, text: ' ', attachments: attachments}

lookupCard :: Text -> IO Card
lookupCard cardName = 
  let opts = defaults & param "name" .~ [cardName]
  in  do 
    r <- asJSON =<< getWith opts "http://api.mtgapi.com/v2/cards"
    pure $ r ^. responseBody

data Card = Card
  { raw :: Value
  , name :: Text
  , multiverseId :: Text
  , gathererImage :: Text
  }

instance FromJSON Card where
  parseJSON o@(Object v) = 
    Card <$> pure o
         <*> v .: "name"
         <*> v .: "multiverseid"
         <*> ((v .: "images") >>= (.: "gatherer"))

  parseJSON _ = mzero

