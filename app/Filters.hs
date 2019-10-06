{-# LANGUAGE OverloadedStrings #-}

module Filters (
    needReview
  , onReview
  , readyToDeploy
  , newDev
  , smartPStrong
  , smartPWeak
) where

import Types

import Control.Lens  hiding ((<&>))
import Control.Monad (liftM2)
import Data.Sort
import Data.List
import Data.Char (toLower)

issuePriority :: Issue -> Integer
issuePriority issue = priority issue & name & priorityToInt

priorityToInt :: String -> Integer
priorityToInt priority
  | priority == "Немедленный" = 0
  | priority == "Срочный" = 1
  | priority == "Высокий" = 2
  | priority == "Нормальный" = 3
  | otherwise = 4

developers :: [String]
developers =
  [ "Дмитрий Дубина"
  , "Anton Evseev"
  , "Игорь Чернов"
  , "Игорь Баранов"
  , "Alexey Golubov"
  , "Александр Голубов"
  , "Михаил Яшков"
  , "Дмитрий Кузнецов"
  , "Ренат Фасхутдинов"
  , "Владислав Ахтямов"
  ]

toLowerS :: String -> String
toLowerS = map toLower

(<|>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<|>) = liftM2 (||)

(<&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<&>) = liftM2 (&&)

devP :: Issue -> Bool
devP issues =
  case assigned_to issues of
    Just assigned -> name assigned `elem` developers
    Nothing       -> False

statusP :: [String] -> Issue -> Bool
statusP statusVal issue = (name . status) issue `elem` statusVal

smartPStrong :: String -> Issue -> Bool
smartPStrong searchText = smartP searchText (<&>) True

smartPWeak :: String -> Issue -> Bool
smartPWeak searchText = smartP searchText (<|>) False

smartP :: String
       -> ((Issue -> Bool)  -> (Issue -> Bool)  -> (Issue -> Bool))
       -> Bool 
       -> Issue -> Bool
smartP searchText op' initial =
  foldl op' (const initial) (map (\w i -> w `isInfixOf` issueToSmartSearch i) (words (toLowerS searchText)))
  where
    issueToSmartSearch issue =
      map
        ($ issue)
        [ subject
        , showMaybe . assigned_to
        , showMaybe . fixed_version
        , showMaybe . description
        , show . status
        , show . author
        , show . iId
        , show . priority
        , show . tracker
        ]
      & concat
      & toLowerS

needReview :: IssueFilter
needReview issues = issues & filter (statusP ["Требует ревью решения"]) & sortOn issuePriority

onReview :: IssueFilter
onReview issues = issues & filter (statusP ["На ревью"]) & sortOn issuePriority

newDev :: IssueFilter
newDev issues = issues & filter (statusP ["Новая", "Принята"] <&> devP) & sortOn issuePriority

readyToDeploy :: IssueFilter
readyToDeploy issues = issues & filter (statusP ["Ожидает деплоя"]) & sortOn issuePriority
