{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.FilePath
import System.Directory
import SiteArchiver.DataStore
import SiteArchiver.Types
import SiteArchiver.JobRunner
import Network.Wai.Middleware.Static
import Control.Monad.IO.Class
import Network.URI
import Network.HTTP.Headers
import Network.HTTP.Types.Status
import SiteArchiver.Url
import Text.HTML.TagSoup
import Data.Maybe
import Data.List
import Data.Aeson(decode)
import Web.Scotty
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as BSC

jobPath :: String
jobPath = "/job/"

main :: IO ()
main = do
  directory <- getCurrentDirectory
  ds <- createDataStore (directory </> "site-archiver.sqlite3")
  scotty 3000 $ do
    get (regex $ concat ["^", jobPath, "([0-9]+)(.*)$"]) $ do
      jobId <- param "1"
      pagePath <- param "2"
      Just startUrlString <- liftIO $ getStartUrlForJob ds jobId
      let Just startUrl = parseAbsoluteURI startUrlString
          Just pathUri = parseURIReference pagePath
          Just siteDomain = getDomain startUrl
          pageUri = relativeTo pathUri siteDomain
      Just response <- liftIO $ loadResponse ds pageUri jobId
      rawResponse $ modifyResponse pageUri jobId response
    post "/resources/job/:siteId" $ do
      siteId <- param "siteId"
      Just site <- liftIO $ getSite ds siteId
      liftIO $ startNewJob ds site
      emptyResponse
    get "/resources/job/:siteId" $ do
      siteId <- param "siteId"
      liftIO (jobsForSite ds siteId) >>= json
    get "/resources/site" (liftIO (allSites ds) >>= json)
    get "/resources/site/:id" $ do
       siteId <- param "id"
       Just site <- liftIO $ getSite ds siteId
       json site
    post "/resources/site" $ do
      siteJSON <- body
      let Just (NewSite name startUrl) = decode siteJSON
      liftIO $ insertSite ds name startUrl
      emptyResponse

    middleware $ staticPolicy $ only [("", "public/index.html")]
    middleware $ staticPolicy (addBase "public")

isHtmlResponse :: SavedResponse -> Bool
isHtmlResponse (SavedResponse _ _ hs _) = (lookup "Content-Type" hs >>= Just . isInfixOf "text/html") == Just True

modifyResponse :: URI -> ArchiveJobId -> SavedResponse -> SavedResponse
modifyResponse pageUrl jobId res@(SavedResponse code _ _ _)
  | code `elem` [301, 302, 303, 307, 308] = modifyRedirect pageUrl jobId res
  | isHtmlResponse res                    = modifyHtmlResponse pageUrl jobId res
  | otherwise                             = res

modifyHtmlResponse :: URI -> ArchiveJobId -> SavedResponse -> SavedResponse
modifyHtmlResponse pageUrl jobId (SavedResponse c r hs b) = SavedResponse c r hs (modifyBody b) where
  modifyBody = renderTagsOptions noEscapeOptions . modifyUrls (getArchiveUrl jobId pageUrl) . parseTags
  noEscapeOptions = RenderOptions id (const False) (const True)

modifyRedirect :: URI -> ArchiveJobId -> SavedResponse -> SavedResponse
modifyRedirect pageUrl jobId (SavedResponse c r hs b) = SavedResponse c r hs' b where
  hs' = fromMaybe hs $ do
    location  <- lookup locationHeader hs >>= parseURIReference
    return $ replaceItem locationHeader (show $ getArchiveUrl jobId pageUrl location) hs
  locationHeader = show HdrLocation

getArchiveUrl :: ArchiveJobId -> URI -> URI -> URI
getArchiveUrl jobId pageUrl url =
  let absoluteUrl = url `relativeTo` pageUrl
      onSameDomain = getDomain absoluteUrl == getDomain pageUrl
  in if onSameDomain then fromJust $ parseURIReference $ concat [jobPath, show jobId, uriPath absoluteUrl, uriQuery absoluteUrl, uriFragment absoluteUrl] else url

emptyResponse :: ActionM ()
emptyResponse = raw ""

rawResponse :: SavedResponse -> ActionM ()
rawResponse (SavedResponse code reason hs b) = do
  let filteredHeaders = filter (\(k,_) -> k /= "Connection" && k /= "Content-Length") hs
  raw $ BSL.fromStrict b
  mapM_ (\(n, v) -> setHeader (TL.pack n) (TL.pack v)) filteredHeaders
  status $ Status code (BSC.pack reason)
