{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module HmBot.MTGAPI 
  where

import           Control.Lens ((&), (.~), (^.))
import           Control.Monad (mzero)

import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..), (.:), (.=), object) 
import           Data.Aeson.Lens
import           Data.Semigroup ((<>))
import qualified Data.Map.Lazy as M
import           Data.Text

import           Text.Parsec (many, manyTill, parse, skipMany)
import           Text.Parsec.Error
import           Text.Parsec.Text
import           Text.Parsec.Char (char, noneOf, anyChar)

import           Network.Wreq

commands :: Text -> Either ParseError [Text]
commands input = 
  parse allBracketed (unpack input) input

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

gathererImageUrl :: Card -> Text
gathererImageUrl c = 
  "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=" <> multiverseId c

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
