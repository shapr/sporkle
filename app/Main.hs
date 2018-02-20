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

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.IO.Class           (liftIO)

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Data.Aeson              hiding (json)
import           Data.String
import           Data.Text               (Text, pack, unpack)
import           Data.Text.Lazy          (toStrict)
import           Data.Time
import           Database.Persist        hiding (get)
import qualified Database.Persist        as P
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH
import           GHC.Generics
import           Lucid
import           Text.Read               (readMaybe)

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
  do pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
     spockCfg <- defaultSpockCfg () (PCPool pool) ()
     runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
     runSpock 8000 (spock spockCfg app)

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

type ApiAction a = SpockAction SqlBackend () () a
type Api = SpockM SqlBackend () () ()

app = do
  get root $ do
    redirect "exercise"
  -- get "exercise" $ do
  --   allExercise <- runSQL $ selectList [] [Asc ExerciseId]
  --   html . toStrict . renderText $ pageTemplate
  --     (do exerciseTemplate allExercise) "some title"
  get "exercise" $ do
    allExercise <- runSQL $ selectList [] [Asc ExerciseId]
    nowTime <- liftIO getCurrentTime
    html . toStrict . renderText $ pageTemplate
      (do h1_ "Exercise List"
          exerciseTemplate allExercise
          h1_ "Do stuff"
          form_ [action_ "exercise", method_ "post"] $ do
            label_ "Name: "
            input_ [type_ "text", name_ "name"]
            label_ "Reps: "
            input_ [type_ "text", name_ "reps"]
            label_ "Time: "
            input_ [type_ "text", name_ "whendo", value_ (pack . show $ nowTime)]
            input_ [type_ "submit"]
      ) "Exercise List"

  post "exercise" $ do
    ps <- params
    let maybeExercise = mbEx ps
    case maybeExercise of
      Just theExercise -> do
        _ <- runSQL $ insert theExercise
        redirect "/exercise"
      Nothing -> errorJson 1 "You screwed up"

  get "exercisefilter" $ do
    ps <- params
    let fil = case lookup "filter" ps of
                Nothing -> []
                Just thefilter -> [Filter ExerciseName (Left thefilter) (BackendSpecificFilter "LIKE")]
    allExercise <- runSQL $ selectList fil [Asc ExerciseId]
    nowTime <- liftIO getCurrentTime
    html . toStrict . renderText $ pageTemplate
      (do h1_ "Exercise List"
          exerciseTemplate allExercise
          h1_ "Do stuff"
          form_ [action_ "exercise", method_ "post"] $ do
            label_ "Name: "
            input_ [type_ "text", name_ "name"]
            label_ "Reps: "
            input_ [type_ "text", name_ "reps"]
            label_ "Time: "
            input_ [type_ "text", name_ "whendo", value_ (pack . show $ nowTime)]
            input_ [type_ "submit"]
      ) "Exercise List"

    let maybeExercise = mbEx ps
    case maybeExercise of
      Just theExercise -> do
        _ <- runSQL $ insert theExercise
        redirect "/exercise"
      Nothing -> errorJson 1 "You screwed up"

  get ("exercise" <//> var) $ \exerciseId -> do
    maybeExercise <- runSQL $ P.get exerciseId :: ApiAction (Maybe Exercise)
    case maybeExercise of
      Nothing          -> errorJson 2 "nope"
      Just theExercise -> json theExercise

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    ["result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

exerciseTemplate xs = do
  table_ $ do
    tr_ $ do
      th_ "Exercise"
    sequence_ $ oneex <$> xs

pageTemplate :: Monad m => HtmlT m a -> Text -> HtmlT m a
pageTemplate x title = do
  doctype_
  html_ $ do
    head_ $ do
      title_ $ toHtml title
    body_ $ do
      x

oneex :: Monad m => Entity Exercise -> HtmlT m ()
oneex (Entity _ f) = do tr_ $
                          do td_ . toHtml $ exerciseName f
                             td_ . toHtml . show $ exerciseReps f
                             td_ . toHtml . show $ exerciseWhendo f

mbEx  :: (Eq a, Data.String.IsString a) => [(a, Text)] -> Maybe Exercise
mbEx d = let upl = flip lookup $ d in
           do n <- upl "name"
              r <- upl "reps"
              mr <- readMaybe $ unpack r
              t <- upl "whendo"
              mt <- readMaybe $ unpack t
              return $ Exercise n mr mt
