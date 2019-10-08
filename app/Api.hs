{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Api where

import Types

import Control.Lens hiding ((<&>))
import Control.Monad (liftM2, mapM, foldM)
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Either
import Data.ByteString.Lazy (ByteString)
import Network.Wreq
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text (Text, pack)

limit' :: Integer
limit' = 100

downloadIssues :: String -> String -> IO [Issue]
downloadIssues username password = do
  body <- download username password shortQuery
  print $ show (formatUrlList body)
  downloadIssues' username password $ formatUrlList body
  where
    shortQuery = "http://r.tender.pro/projects/newsys/issues.json?limit=1"
    baseUrl = "http://r.tender.pro/projects/newsys/issues.json?limit=" ++ show limit'
  
    download :: String -> String -> String -> IO ByteString 
    download username password url = do
      let opts = defaults & auth ?~ basicAuth (encodeUtf8 . pack $ username) (encodeUtf8 . pack $ password)
      print ("Start download url " ++ url)
      r <- getWith opts url
      return $ r ^. responseBody
  
    downloadIssues' :: String -> String -> Maybe [String] -> IO [Issue]
    downloadIssues' _ _ Nothing = return []
    downloadIssues' username password (Just urls) = do
      roots <- mapM (download username password) urls
      let parseRoots = map (\r -> eitherDecode r :: Either String Root) roots
      
      let badRoots = lefts parseRoots
      let okRoots = rights parseRoots & map issues
      
      mapM_ print badRoots
      
      return $ concat okRoots

    formatUrlList body = do
      root <- decode body :: Maybe Root
      let pageCount = total_count root `div` limit'
      return $ map (formatUrl baseUrl "offset" . (* limit')) [0 .. pageCount + 1]
      where
        formatUrl url k v = url ++ "&" ++ k ++ "=" ++ show v

--TODO: Перетестировать таску с заказами
--TODO: Узнать у димы про типы полей