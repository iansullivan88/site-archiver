module SiteArchiver.SiteCrawler(crawl) where

import Network.URI
import Network.HTTP
import qualified Data.ByteString as BS
import Data.Set (Set)
import Data.List
import qualified Data.Set as Set
import Control.Monad.State.Lazy
import Control.Applicative
import Text.HTML.TagSoup
import SiteArchiver.Url
import Data.Maybe
import Data.Functor.Identity

type ArchiveState = (Set URI, [URI])
type URLHandler = URI -> Response BS.ByteString -> IO ()

crawl :: URI -> URLHandler -> IO ()
crawl startUrl f = do
  let crawlableDomains = Set.singleton $ fromJust $ getDomain startUrl
      (_, initialState) = runIdentity $ runStateT (addUrlsToProcess [startUrl]) (Set.empty, [])
  void $ while (\(_, tp) -> (not.null) tp) initialState $ do
    nextUrl <- popNextUrlToProcess
    newUrls <- liftIO $ processUrl nextUrl crawlableDomains f
    addUrlsToProcess newUrls


popNextUrlToProcess :: (Monad m) => StateT ArchiveState m URI
popNextUrlToProcess = do
  (discoveredUrls, u:us) <- get
  put (discoveredUrls, us)
  return u

addUrlsToProcess :: (Monad m) => [URI] -> StateT ArchiveState m ()
addUrlsToProcess newUrls = do
  let cleanNewUrls = map getInvariantUrl newUrls
  (discoveredUrls, toProcess) <- get
  let newUniqueUrls = Set.difference (Set.fromList cleanNewUrls) discoveredUrls
  put (Set.union discoveredUrls newUniqueUrls, toProcess ++ Set.toList newUniqueUrls)


while :: (Monad m) => (s -> Bool) -> s -> StateT s m a -> m s
while f currentState stateCalculation =
  if f currentState then do
    (_, newState) <- runStateT stateCalculation currentState
    while f newState stateCalculation
  else return currentState


processUrl :: URI -> Set URI -> URLHandler -> IO [URI]
processUrl url domains f = do
  result <- simpleHTTP $ mkRequest GET url
  case result of
     Left _    -> return []
     Right res -> do
       f url res
       return $ filter isAllowedDomain $ filter isHttpUrl $  map (`relativeTo` url) $ getLinkedUrls url res where
          isAllowedDomain u = fromMaybe False (flip Set.member domains <$> getDomain u)

getLinkedUrls :: URI -> Response BS.ByteString -> [URI]
getLinkedUrls _ (Response (2,_,_) _ hs body) = case lookupHeader HdrContentType hs of
  Just contentType -> if "text/html" `isInfixOf` contentType then getUrls $ parseTags body else []
  _ -> []
getLinkedUrls url (Response (3,_,_) _ hs _) = maybeToList $ lookupHeader HdrLocation hs >>= parseURIReference >>= \u -> Just (u `relativeTo` url)
getLinkedUrls _ _ = []
