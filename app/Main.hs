{-# LANGUAGE OverloadedStrings #-}

import Api
import Filters
import Types

import Control.Applicative ((<|>))
import Control.Lens hiding ((<&>))
import Control.Monad.Trans (liftIO)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO as T
import Data.IORef
import Data.ByteString.UTF8 (fromString)
import System.IO.Unsafe
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, liftM2)
import System.Timeout (timeout)
import Data.Maybe (isJust)
import Data.Set (fromList, toList)
import System.Environment (getEnv)

{-# NOINLINE cache #-}
cache :: IORef State
cache = unsafePerformIO $ newIORef (State [] [])

echoBot :: BotApp [Issue] Action
echoBot =
  BotApp
    { botInitialModel = []
    , botAction = flip updateToAction
    , botHandler = handleAction
    , botJobs = []
    }

updateToAction :: [Issue] -> Update -> Maybe Action
updateToAction _ =
  parseUpdate $
        NeedReview    <$> command "need_review"
    <|> NeedReview    <$> command "need_review@TestFsharpBot"
    <|> OnReview      <$> command "on_review"
    <|> OnReview      <$> command "on_review@TestFsharpBot"
    <|> NewDev        <$> command "new_dev"
    <|> NewDev        <$> command "new_dev@TestFsharpBot"
    <|> ReadyToDeploy <$> command "ready_to_deploy"
    <|> ReadyToDeploy <$> command "ready_to_deploy@TestFsharpBot"
    <|> S             <$> command "s"
    <|> S             <$> command "s@TestFsharpBot"
    <|> SS            <$> command "ss"
    <|> SS            <$> command "ss@TestFsharpBot"
    <|> Help          <$  command "help"
    <|> Help          <$  command "help@TestFsharpBot"
    

replyM :: String -> BotM ()
replyM msg = reply $ ReplyMessage (pack msg) (Just HTML) (Just True) Nothing Nothing Nothing

replyIssues :: [Issue] -> IssueFilter -> BotM ()
replyIssues issues filter' = 
    issues 
  & filter'
  & take 10
  & joinNL
  & replyM

handleAction :: Action -> [Issue] -> Eff Action [Issue]
handleAction action model =
  case action of
    NoOp -> pure model
    S msg ->
      model <# do
        issues <- liftIO $ issuesList <$> readIORef cache
        replyIssues issues (filter (smartPWeak (unpack msg)))
        return NoOp
    SS msg ->
      model <# do
        issues <- liftIO $ issuesList <$> readIORef cache
        replyIssues issues (filter (smartPStrong (unpack msg)))
        return NoOp
    NeedReview msg ->
      model <# do
        issues <- liftIO $ issuesList <$> readIORef cache
        replyIssues issues (needReview . filter (smartPStrong (unpack msg)))
        return NoOp
    OnReview msg ->
      model <# do
        issues <- liftIO $ issuesList <$> readIORef cache
        replyIssues issues (onReview . filter (smartPStrong (unpack msg)))
        return NoOp
    NewDev msg ->
      model <# do
        issues <- liftIO $ issuesList <$> readIORef cache
        replyIssues issues (newDev . filter (smartPStrong (unpack msg)))
        return NoOp
    ReadyToDeploy msg ->
      model <# do
        issues <- liftIO $ issuesList <$> readIORef cache
        replyIssues issues (readyToDeploy . filter (smartPStrong (unpack msg)))
        return NoOp
    Help ->
      model <# do
        replyM "Фильтр по задачам из редмайна. Список задач обновляется раз в 3 минуты.\n \ 
                \Список сохраняется в кеш, можно не бояться делать много запросов.\n \ 
                \<b>Ограничение на вывод задач 10 штук. Для более подробного вывода, используйте фильтры</b>\n \ 
                \\n \ 
                \Список возможностей:\n \ 
                \* Свободный поиск по задачам, команды <b>/s</b>, <b>/ss</b>.\n \ 
                \  <b>/s</b> - Поиск задач, в качестве предиката используется \"ИЛИ\".\n \ 
                \  Например: <code>/s 1.5 Ожидает деплоя</code> - найдет все задачи из версии 1.5 или содержащие слова: Ожидает, деплоя.\n \ 
                \  <b>/ss</b> - аналог /s но предикат \"И\".\n \ 
                \  Например: <code>/ss 1.5 Ожидает деплоя</code> - найдет все задачи из версии 1.5 ожидающие деплоя\n \ 
                \\n \ 
                \  Фильтровать можно по описанию, версии, статусу, автору, назначенному, id, трекеру, приоритету\n \ 
                \\n \ 
                \* Заранее подготовленные фильтры\n \ 
                \  Все фильтры поддерживают семантику <b>ss</b>\n \ 
                \\n \ 
                \  * <b>/need_review</b> - Список задач, ожидающих ревью\n \ 
                \  * <b>/on_review</b> - Список задач на ревью\n \ 
                \  * <b>/new_dev</b> - Новые задачи, назначенные на наш отдел\n \ 
                \  * <b>/ready_to_deploy</b> - Задачи, готовые к деплою"
        return NoOp

run :: IO ()
run = do
  username <- getEnv "TELEBOT_USERNAME"
  password <- getEnv "TELEBOT_PASSWORD"
  token <- getEnv "TELEBOT_TOKEN"

  forkIO $ forever $ do
    print "Loop..."
    tasks <- downloadIssues username password
    let versions = tasks 
                   & map fixed_version
                   & filter isJust
                   & map showMaybe
                   & toList . fromList
    
    writeIORef cache State {issuesList = tasks, versions = versions}
    threadDelay (1000000 * 3 * 60)

  env <- defaultTelegramClientEnv (Token (pack token))
  startBot_ (conversationBot updateChatId echoBot) env

main :: IO ()
main = run
