module SiteArchiver.JobRunner(startNewJob) where

import Network.URI
import Network.HTTP
import qualified Data.ByteString as BS
import SiteArchiver.DataStore
import SiteArchiver.Types
import Data.Time.Clock.POSIX
import SiteArchiver.SiteCrawler

startNewJob :: DataStore -> Site -> IO ()
startNewJob ds (Site siteId _ url) = do
  time <- getPOSIXTime
  jobId <- createJob ds time siteId
  crawl url (urlHandler ds jobId)


urlHandler :: DataStore -> ArchiveJobId -> URI -> Response BS.ByteString -> IO ()
urlHandler ds jobId url res = do
  time <- getPOSIXTime
  saveResponse ds (convertResponse res) jobId time url where

convertResponse :: Response BS.ByteString -> SavedResponse
convertResponse (Response c r hs b) = SavedResponse (convertCode c) r (convertHeaders hs) b where
  convertCode (x,y,z) = 100*x + 10*y + z
  convertHeaders = map (\h -> (show $ hdrName h, hdrValue h))
