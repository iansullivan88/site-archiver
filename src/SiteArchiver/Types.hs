{-# LANGUAGE OverloadedStrings #-}

module SiteArchiver.Types(SavedResponse(SavedResponse), ArchiveJobId, ResponseTime, Site(Site), NewSite(NewSite), ArchiveJob(ArchiveJob)) where

import qualified Data.ByteString as BS
import Network.URI
import Data.Time.Clock.POSIX
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Data.Maybe

type ArchiveJobId = Int
type ResponseTime = Int

data SavedResponse = SavedResponse Int String [(String, String)] BS.ByteString

data Site = Site Int String URI

instance ToJSON Site where
  toJSON (Site siteId name startUrl) = object ["id" .= siteId, "name" .= name, "startUrl" .= show startUrl]

instance FromJSON Site where
  parseJSON (Object s) = Site <$> s .: "id" <*> s .: "name" <*> parseUrl (s .: "startUrl")
  parseJSON _ = error "Could not deserialise site"

data NewSite = NewSite String URI

instance FromJSON NewSite where
  parseJSON (Object s) = NewSite <$> s .: "name" <*> parseUrl (s .: "startUrl")
  parseJSON _ = error "Could not deserialise site"


data ArchiveJob = ArchiveJob ArchiveJobId POSIXTime Int

instance ToJSON ArchiveJob where
  toJSON (ArchiveJob jobId time siteId) = object ["id" .= jobId, "time" .= timeStamp, "siteId" .= siteId] where
    timeStamp = show (round time :: Integer)


parseUrl :: Parser String -> Parser URI
parseUrl = fmap (fromJust . parseAbsoluteURI)
