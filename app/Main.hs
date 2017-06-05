{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Lib
import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Control.Monad.Trans
import           Data.Aeson              hiding (json)
import           Data.IORef
import           Data.Monoid
import           Data.Monoid
import           Data.Text               (Text, pack)
import qualified Data.Text               as T
import           Data.Text.Lazy          (toStrict)
import           Data.Time
import           Database.Persist        hiding (get)
import qualified Database.Persist        as P
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH
import           GHC.Generics
import           Lucid

{- GET /exercise - list all exercises rows
POST /exercise - insert new exercise row
GET /exercise/id - get one single exercise by id
PUT /exercise/id - edit one single exercise by id
DELETE /exercise/id - delete one exercise by id -}
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Exercise json -- create ToJSON and FromJSON instances automagically
  name Text
  reps Int
  whendo UTCTime
  deriving Show Generic
|]
-- data Exercise = Exercise {
--   name     :: Text -- more tags later? muscle groups, etc? YAGNI
--   , reps   :: Int
--   , whendo :: UTCTime
--   } deriving Generic

main :: IO ()
main =
  do ref <- newIORef 0
     pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
     spockCfg <- defaultSpockCfg () (PCPool pool) ()
     runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
     runSpock 8000 (spock spockCfg app)

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

type ApiAction a = SpockAction SqlBackend () () a
type Api = SpockM SqlBackend () () ()

app = do
  get root $ do
    html . toStrict . renderText $ pageTemplate
      (do
          p_ "hi!") "SpockExample"
  get "test" $ do
    html . toStrict . renderText $ pageTemplate
      (do
        p_ "just a test") "some title"
  get "exercise" $ do
    allExercise <- runSQL $ selectList [] [Asc ExerciseId]
    -- json allExercise
    html . toStrict . renderText $ pageTemplate
      (do exerciseTemplate allExercise) "some title"
  get ("exercise" <//> var) $ \exerciseId -> do
    maybeExercise <- runSQL $ P.get exerciseId :: ApiAction (Maybe Exercise)
    case maybeExercise of
      Nothing          -> errorJson 2 "nope"
      Just theExercise -> json theExercise
  post "exercise" $ do
    maybeExercise <- jsonBody :: ApiAction (Maybe Exercise)
    case maybeExercise of
      Nothing -> errorJson 1 "Failed to parse request body"
      Just theExercise -> do
        newId <- runSQL $ insert theExercise
        json $ object ["result" .= String "success", "id" .= newId]

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    ["result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

-- exerciseTemplate :: Monad m => HtmlT m a -> Text -> HtmlT m a
exerciseTemplate xs = do
  table_ $ do
    tr_ $ do
      th_ "Exercise"
    sequence_ $ oneex <$> xs

-- guestsTemplate :: Monad m => [a] -> HtmlT m ()
-- guestsTemplate xs = do
--   table_ $ do
--     tr_ $ do
--       th_ "Guest"
--     sequence_ $ map f xs
--   where f :: Monad m => Only Text -> HtmlT m ()
--         f (Entity _ f) = do
--           tr_ $ do
--             td_ $ toHtml x

pageTemplate :: Monad m => HtmlT m a -> Text -> HtmlT m a
pageTemplate x title = do
  doctype_
  html_ $ do
    head_ $ do
      title_ $ toHtml title
    body_ $ do
      x

foop :: IO ()
foop = runSqlite "api.db" $ do
  runMigration migrateAll
  things <- selectList [] [Asc ExerciseId]
  liftIO $ print $ whendo <$> things

-- finally
oneex :: Monad m => Entity Exercise -> HtmlT m ()
oneex (Entity e f) = do tr_ $
                          do td_ . toHtml $ exerciseName f
                             td_ . toHtml . show $ exerciseReps f
                             td_ . toHtml . show $ exerciseWhendo f

whendo (Entity e f) = toHtml . show $ exerciseWhendo f
