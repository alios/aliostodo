{-# LANGUAGE TemplateHaskell, TupleSections, OverloadedStrings, FlexibleContexts #-}
module Handler.Api (getPublishR, postPublishR) where

import Data.Time.Clock
import Data.Time.LocalTime

import Import
import Yesod.Auth
import Text.Blaze (Markup)

getPublishR :: Handler RepHtml
getPublishR = do 
  auid <- requireAuthId
  t <- liftIO $ getCurrentTime
  (formWidget, formEnctype) <- generateFormPost (todoForm auid t t)
  let submission = Nothing :: Maybe (Text, Int)
      handlerName = "getPublishR" :: Text
  defaultLayout $ do
    setTitle "Publish new ToDo item"
    $(widgetFile "publishTodo")

postPublishR :: Handler ()
postPublishR = do
  auid <- requireAuthId
  t <- liftIO $ getCurrentTime
  ((res, _), _) <- runFormPost (todoForm auid t t)
  case (res) of
    FormMissing -> undefined
    FormFailure ts -> undefined
    FormSuccess i -> liftIO $ print i

utcT Nothing _ = Nothing
utcT (Just d) Nothing = utcT (Just d) (Just $ TimeOfDay 12 0 0)
utcT (Just d) (Just t) = Just $ UTCTime d (timeOfDayToTime t)


todoForm uid tsc tsu = 
  let f tit t u l endD endT = 
        TodoItem uid tit (unTextarea t) u l (utcT endD endT) tsc tsu
  in renderDivs $ f
  <$> areq textField "Title" Nothing
  <*> areq textareaField "Description" Nothing
  <*> areq intField "Use" Nothing
  <*> areq intField "Length" Nothing
  <*> aopt dayField "End Day" Nothing
  <*> aopt timeField "End Time" Nothing
           

