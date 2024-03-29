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
    { iId             :: Integer
    , project         :: Entity
    , tracker         :: Entity
    , status          :: Entity
    , priority        :: Entity
    , author          :: Entity
    , assigned_to     :: Maybe Entity
    , fixed_version   :: Maybe Entity
    , subject         :: String
    , description     :: Maybe String
    , start_date      :: Maybe String
    , estimated_hours :: Maybe Float
    , created_on      :: String
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

formatOrEmpty :: Show a => String -> Maybe a -> String -> String 
formatOrEmpty prefix (Just e) suffix = prefix ++ show e ++ suffix
formatOrEmpty _ Nothing _ = "" 

instance Show Issue where
  show issue = 
         "| <b>" ++ (show . priority $ issue) ++ "</b> " ++ (show . status $ issue)  ++ " " ++ (show . tracker $ issue)
    ++ "\n| " ++ (formatDate . created_on $ issue) ++ formatOrEmpty " (оценка " (estimated_hours issue) " ч)"
    ++ "\n| " ++ "http://r.tender.pro/issues/" ++ (show . iId $ issue)
    ++ "\n| " ++ (show . author $ issue) ++ " -> " ++ (showMaybe . assigned_to $ issue)
    ++ formatOrEmpty "\n| " (fixed_version issue) ""
    ++ "\n| " ++ subject issue

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
  | D
