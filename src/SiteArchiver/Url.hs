{-# LANGUAGE OverloadedStrings #-}

module SiteArchiver.Url where

import Control.Monad
import Data.Maybe
import Network.URI
import Text.HTML.TagSoup
import Text.StringLike
import Data.Char
import qualified Data.ByteString as BS

getDomain :: URI -> Maybe URI
getDomain u = do
  a <- uriAuthority u
  parseAbsoluteURI $ concat [uriScheme u, "//", uriRegName a, uriPort a]

getUrls :: [Tag BS.ByteString] -> [URI]
getUrls = mapMaybe (fmap snd . getUrlFromTag) where

modifyUrls :: (URI -> URI) -> [Tag BS.ByteString] -> [Tag BS.ByteString]
modifyUrls f = map modifyTag where
  modifyTag t@(TagOpen n attrs) = case getUrlFromTag t of
    Just (key, uri) -> let newUri = fromString $ show $ f uri in TagOpen n (replaceItem key newUri attrs)
    Nothing         -> t
  modifyTag x = x

getUrlFromTag :: Tag BS.ByteString -> Maybe (BS.ByteString, URI)
getUrlFromTag (TagOpen "img" attrs) = getAttributeAndUrl "src" attrs
getUrlFromTag (TagOpen "script" attrs) = getAttributeAndUrl "src" attrs
getUrlFromTag (TagOpen "a" attrs) = getAttributeAndUrl "href" attrs
getUrlFromTag (TagOpen "link" attrs) = getAttributeAndUrl "href" attrs
getUrlFromTag _ = Nothing

getAttributeAndUrl :: BS.ByteString -> [Attribute BS.ByteString] -> Maybe (BS.ByteString, URI)
getAttributeAndUrl attr attrs = liftM toString (lookup attr attrs) >>= parseURIReference >>= \u -> return (attr, u)

getInvariantUrl :: URI -> URI
getInvariantUrl url = url{uriFragment = ""}

isHttpUrl :: URI -> Bool
isHttpUrl u = case map toLower $ uriScheme u of
  "http:"    -> True
  "https:"   -> True
  _          -> False

replaceItem :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
replaceItem _ _ []     = []
replaceItem k v ((x,y):xys)
  | x == k  = (x,v):xys
  | otherwise = (x,y) : replaceItem k v xys
