{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE OverloadedStrings  #-}

module Types where

import Data.Aeson.TH
import Data.Text (Text) 
import Data.Time.ISO8601

data Entity =
  Entity
    { id   :: Integer
    , name :: String
    }
  deriving (Eq)

instance Show Entity where
  show = name 

data Issue =
  Issue
    { iId           :: Integer
    , project       :: Entity
    , tracker       :: Entity
    , status        :: Entity
    , priority      :: Entity
    , author        :: Entity
    , assigned_to   :: Maybe Entity
    , fixed_version :: Maybe Entity
    , subject       :: String
    , description   :: Maybe String
    , start_date    :: String
    , created_on    :: String
    }
  deriving (Eq)
  
formatDate :: String -> String
formatDate = showMaybe . parseISO8601

joinNL :: Show a => [a] -> String
joinNL [] = "Пустой результат"
joinNL ss = foldl (\acc i -> acc ++ (show i ++ "\n\n")) "" ss

showMaybe :: Show a => Maybe a -> String
showMaybe (Just s) = show s
showMaybe Nothing = "---"

instance Show Issue where
  show issue = 
         "| <b>" ++ (show . priority $ issue) ++ "</b> " ++ (formatDate . created_on $ issue)
    ++ "\n| " ++ "http://r.tender.pro/issues/" ++ (show . iId $ issue)
    ++ "\n| " ++ subject issue
    ++ "\n| " ++ (show . status $ issue)
    ++ "\n| " ++ (show . author $ issue) ++ " -> " ++ (showMaybe . assigned_to $ issue)

data Root =
  Root
    { issues      :: [Issue]
    , total_count :: Integer
    , offset      :: Integer
    , limit       :: Integer
    }
  deriving (Show, Eq)

$(deriveFromJSON defaultOptions ''Root)

$(deriveFromJSON
    defaultOptions
      { fieldLabelModifier =
          \case
            "id" -> "iId"
            "iId" -> "id"
            label -> label
      }
    ''Issue)

$(deriveFromJSON defaultOptions ''Entity)

type IssueFilter = [Issue] -> [Issue]

type Predicate a = a -> Bool
type IssuePredicate = Predicate Issue

data State = 
  State
    { issuesList :: [Issue]
    , versions :: [String]
    }
  deriving (Eq, Show)

data Action
  = NoOp
  | NeedReview Text
  | OnReview Text
  | NewDev Text
  | ReadyToDeploy Text
  | S Text
  | SS Text
  | Help
