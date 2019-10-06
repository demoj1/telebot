{-# LANGUAGE OverloadedStrings #-}

module Api where

import Types

import Control.Lens hiding ((<&>))
import Control.Monad (liftM2)
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Network.Wreq
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text (Text, pack)

downloadIssues :: String -> String -> IO [Issue]
downloadIssues username password = do
  body <- download username password baseUrl
  downloadIssues' username password $ formatUrlList body
  where
    baseUrl = "http://r.tender.pro/projects/newsys/issues.json"
  
    download username password url = do
      let opts = defaults & auth ?~ basicAuth (encodeUtf8 . pack $ username) (encodeUtf8 . pack $ password)
      print ("Start download url " ++ url)
      r <- getWith opts url
      return $ r ^. responseBody
  
    downloadIssues' _ _ Nothing = return []
    downloadIssues' username password (Just urls) = do
      roots <- mapM (download username password) urls
      return $ concatMap (foldl (\acc r -> acc ++ issues r) [] . (\r -> decode r :: Maybe Root)) roots

    formatUrlList body = do
      root <- decode body :: Maybe Root
      let pageCount = total_count root `div` limit root
      return $ map (formatUrl baseUrl "offset" . (* 25)) [0 .. pageCount]
      where
        formatUrl url k v = url ++ "?" ++ k ++ "=" ++ show v
