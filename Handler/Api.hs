{-# LANGUAGE TemplateHaskell, TupleSections, OverloadedStrings #-}
module Handler.Api (getPublishR, postPublishR) where

import Data.Time.Clock
import Data.Time.LocalTime

import Import


getPublishR :: Handler RepHtml
getPublishR = do 
  (formWidget, formEnctype) <- generateFormPost 
                               (todoForm undefined undefined undefined)
  let submission = Nothing :: Maybe (Text, Int)
      handlerName = "getPublishR" :: Text
  defaultLayout $ do
    setTitle "Publish new ToDo item"
    $(widgetFile "publishTodo")

postPublishR :: Handler ()
postPublishR = undefined


utcT Nothing _ = Nothing
utcT (Just d) Nothing = utcT (Just d) (Just $ TimeOfDay 12 0 0)
utcT (Just d) (Just t) = Just $ UTCTime d (timeOfDayToTime t)

--todoForm :: Form TodoItem

todoForm uid tsc tsu = 
  let f t u l endD endT = 
        TodoItem uid (unTextarea t) u l (utcT endD endT) tsc tsu
  in renderDivs $ f
  <$> areq textareaField "Description" Nothing
  <*> areq intField "Use" Nothing
  <*> areq intField "Length" Nothing
  <*> aopt dayField "End Day" Nothing
  <*> aopt timeField "End Time" Nothing
           

