{-# LANGUAGE ExistentialQuantification #-}

module SiteArchiver.DataStore(DataStore, createDataStore, disconnectDataStore, saveResponse, createJob, loadResponse, getStartUrlForJob, getSite, allSites, insertSite, jobsForSite) where

import Control.Monad
import Database.HDBC
import Database.HDBC.Sqlite3
import SiteArchiver.Types
import Data.Time.Clock.POSIX
import Data.Maybe
import Network.URI
import Data.ByteString.Char8(unpack, readInt)
import qualified Data.ByteString as BS
import qualified Crypto.Hash.MD5 as MD5

data DataStore = forall conn. IConnection conn => DataStore conn

createDataStore :: FilePath -> IO DataStore
createDataStore dbPath = do
  conn <- connectSqlite3 dbPath
  createTables conn
  return $ DataStore conn

disconnectDataStore :: DataStore -> IO ()
disconnectDataStore (DataStore conn) = disconnect conn


createTables :: (IConnection c) => c -> IO ()
createTables conn = do
  mapM_ (runRaw conn) [createSiteSql, createArchiveJobSql, createResponseSql, createResponseBodySql]
  commit conn

getStartUrlForJob :: DataStore -> ArchiveJobId -> IO (Maybe String)
getStartUrlForJob (DataStore conn) jobId = do
  startUrlResponse <- quickQuery' conn "SELECT StartUrl FROM Site s JOIN ArchiveJob b ON s.Id = b.SiteId WHERE b.Id = ?" [SqlInt32 $ fromIntegral jobId]
  return $ case startUrlResponse of
    [[SqlByteString s]] -> Just (unpack s)
    _               -> Nothing

getSite :: DataStore -> Int -> IO (Maybe Site)
getSite ds siteId = fmap (listToMaybe . filter matchesId) (allSites ds) where
  matchesId (Site s _ _) = s == siteId

allSites :: DataStore -> IO [Site]
allSites (DataStore conn) = do
  sites <- quickQuery' conn "SELECT Id, Name, StartUrl FROM Site" []
  return $ map parseSite sites where
    parseSite [siteId, SqlByteString name, SqlByteString url] = Site (fromSql siteId) (unpack name) (fromJust $ parseAbsoluteURI $ unpack url)

insertSite :: DataStore -> String -> URI -> IO ()
insertSite (DataStore conn) name startUrl =
  run conn "INSERT INTO Site (Name, StartUrl) VALUES (?, ?)" [SqlString name, SqlString $ show startUrl] >> commit conn

jobsForSite :: DataStore -> Int -> IO [ArchiveJob]
jobsForSite (DataStore conn) siteId = do
  jobs <- quickQuery' conn "SELECT Id, Time FROM ArchiveJob WHERE SiteId = ?" [SqlInt32 $ fromIntegral siteId]
  return $ map parseJob jobs where
    parseJob [idValue, timeValue] = ArchiveJob (fromSql idValue) (fromSql timeValue) siteId

saveResponse :: DataStore -> SavedResponse -> ArchiveJobId -> POSIXTime -> URI -> IO ()
saveResponse (DataStore conn) (SavedResponse code reason headers body) jobId time url = do
  let hash = MD5.hash body
  existingBody <- getSavedBody conn hash
  -- Save body if it is not already saved
  case existingBody of
    Nothing -> void $ run conn "INSERT INTO ResponseBody (Hash, Body) VALUES (?, ?)" [SqlByteString hash, SqlByteString body]
    Just _  -> return ()
  --save Response
  void $ run conn "INSERT INTO Response (Url, Time, Headers, Reason, Code, BodyHash, ArchiveJobId) VALUES (?,?,?,?,?,?,?)"
    [SqlString $ show url, SqlPOSIXTime time, SqlString $ serialiseHeaders headers, SqlString reason, SqlInt32 $ fromIntegral code, SqlByteString hash, SqlInt32 $ fromIntegral jobId]
  commit conn

loadResponse :: DataStore -> URI -> ArchiveJobId -> IO (Maybe SavedResponse)
loadResponse (DataStore conn) url jobId = do
  responseData <- quickQuery' conn "SELECT Headers, Reason, Code, Body FROM  Response r JOIN ResponseBody b on r.BodyHash = b.Hash WHERE ArchiveJobId = ? AND Url = ?" [SqlInt32 $ fromIntegral jobId, SqlString $ show url]
  return $ case responseData of
    [[SqlByteString h, SqlByteString r, SqlByteString c, SqlByteString b]] -> parseSqlResponse h r c b
    _ -> Nothing
    where
  parseSqlResponse h r c b = do
    code <- fmap fst (readInt c)
    let headers = parseHeaders (unpack h)
    let reason = unpack r
    Just $ SavedResponse code reason headers b


serialiseHeaders :: [(String, String)] -> String
serialiseHeaders = unlines . map (\(k, v) -> k ++ ':':v)

parseHeaders :: String -> [(String, String)]
parseHeaders = map (splitAtElement ':') . lines

-- splitAtElement 4 [2,3,4,5,6,4,3] = ([2,3], [5,6,4,3])
splitAtElement :: (Eq a) => a -> [a] -> ([a], [a])
splitAtElement _ []     = ([], [])
splitAtElement m (x:xs)
  | x == m    = ([],xs)
  | otherwise = let (xs', xs'') = splitAtElement m xs in (x:xs', xs'')

createJob :: DataStore -> POSIXTime -> Int -> IO Int
createJob (DataStore conn) time siteId = do
  void $ run conn "INSERT INTO ArchiveJob (Time, SiteId) VALUES (?,?)" [SqlPOSIXTime time, SqlInt32 $ fromIntegral siteId]
  [[SqlInt64 jobId]] <- quickQuery' conn "SELECT last_insert_rowid()" []
  commit conn
  return $ fromIntegral jobId


getSavedBody :: IConnection a => a -> BS.ByteString -> IO (Maybe BS.ByteString)
getSavedBody conn hash = do
  existingBody <- quickQuery' conn "SELECT Body FROM ResponseBody WHERE Hash = ?" [SqlByteString hash]
  case existingBody of
    [[SqlByteString body]] -> return $ Just body
    _        -> return Nothing



-- TABLE CREATION

createSiteSql :: String
createSiteSql = "CREATE TABLE IF NOT EXISTS Site( \
  \Id INTEGER,\
  \Name TEXT,\
  \StartUrl TEXT,\
  \PRIMARY KEY(Id)\
  \)"

createArchiveJobSql :: String
createArchiveJobSql = "CREATE TABLE IF NOT EXISTS ArchiveJob( \
  \Id       INTEGER PRIMARY KEY,\
  \Time     INTEGER,\
  \SiteId   INTEGER,\
  \FOREIGN KEY(SiteId) REFERENCES Site(Id)\
  \)"

createResponseSql :: String
createResponseSql = "CREATE TABLE IF NOT EXISTS Response( \
  \Url         TEXT,\
  \Time        INTEGER,\
  \Headers     TEXT,\
  \Reason      TEXT,\
  \Code        TEXT,\
  \BodyHash    BLOB,\
  \ArchiveJobId INTEGER,\
  \FOREIGN KEY(ArchiveJobId) REFERENCES ArchiveJob(Id)\
  \FOREIGN KEY(BodyHash)    REFERENCES ResponseBody(Id)\
  \)"

createResponseBodySql :: String
createResponseBodySql = "CREATE TABLE IF NOT EXISTS ResponseBody( \
  \Hash BLOB,\
  \Body BLOB,\
  \PRIMARY KEY(Hash)\
  \)"
