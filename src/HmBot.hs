{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module HmBot 
  --(runApp, app) 
  where

import           Control.Lens ((&), (.~))
import           Control.Monad.Except
import           Control.Monad.IO.Class

import           Data.Aeson (Value(..), object, (.=))
import           Data.Either.Combinators (mapLeft)
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

data HmError 
  = ParseFailure [Message]
  | NotFound [Text]
  
     
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

handlePost :: SlackPost -> ExceptT HmError IO Text
handlePost p = do
  cardNames <- ExceptT . pure $ mapLeft (ParseFailure . errorMessages) $ commands p
  cardData  <- lift lookupCard  

commands :: SlackPost -> Either ParseError [Text]
commands p = parse allBracketed (text p) (text p)

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

--{
-- "query":[{"command":"where","key":"name","conditional":"=","value":"Lightning Bolt"}],
-- "cards":[
-- {"artist":"Christopher Rush",
--  "border":"black","cmc":1,"colors":["Red"],"flavor":null,"foreignNames":null,"hand":null,
--  "images":{
--    "gatherer":"http:\/\/gatherer.wizards.com\/Handlers\/Image.ashx?type=card&multiverseid=209",
--    "mtgimage":"http:\/\/mtgimage.com\/set\/LEA\/lightning bolt.jpg"
--   },"layout":"normal","legalities":[{"format":"Commander","legality":"Legal"},{"format":"Freeform","legality":"Legal"},{"format":"Legacy","legality":"Legal"},{"format":"Modern","legality":"Legal"},{"format":"Prismatic","legality":"Legal"},{"format":"Singleton 100","legality":"Legal"},{"format":"Tribal Wars Legacy","legality":"Legal"},{"format":"Vintage","legality":"Legal"}],"life":null,"links":{"set":"http:\/\/api.mtgapi.com\/v2\/sets?code=LEA"},"loyalty":null,"manaCost":"{R}","multiverseid":209,"name":"Lightning Bolt","names":null,"number":null,"originalText":"Lightning Bolt does 3 damage to one target.","originalType":"Instant","power":null,"printings":["LEA","LEB","2ED","CED","CEI","3ED","4ED","pJGP","ATH","BTD","pMPR","MED","M10","M11","PD2","MM2"],"rarity":"Common","rulings":null,"set":"LEA","subtypes":null,"supertypes":null,"text":"Lightning Bolt deals 3 damage to target creature or player.","toughness":null,"type":"Instant","types":["Instant"],"variations":null,"watermark":null}],"total":16,"perPage":20,"links":{"first":"http:\/\/api.mtgapi.com\/v2\/cards?name=Lightning Bolt","previous":null,"current":"http:\/\/api.mtgapi.com\/v2\/cards?page=1&name=Lightning Bolt","next":null,"last":"http:\/\/api.mtgapi.com\/v2\/cards?page=1&name=Lightning Bolt"}}

lookupCard :: Text -> IO Card
lookupCard cardName = do
  let opts = defaults & param "name" .~ [cardName]
  body <- asJSON =<< getWith opts "http://api.mtgapi.com/v2/cards"
  pure $ 
  let cardRecord = cardData ^. (key "cards" . nth 0)
      multiverseId = cardRecord ^. key "multiverseid"
      gathererImage = cardRecord ^. (key "images" . key "gatherer")
  in  Card <$> cardRecord <*> multiverseId <*> gathererImage

data Card = Card
  { raw :: Value
  , multiverseId :: Text
  , gathererImage :: Text
  }

  
