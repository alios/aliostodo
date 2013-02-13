{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleContexts, GADTs, StandaloneDeriving #-}

module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time.Clock
import Database.Persist.Quasi
import Data.List 

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

deriving instance Show TodoItem
deriving instance Eq TodoItem

x u = UserId ==. u

todoItemV :: UTCTime -> TodoItem -> Double
todoItemV t i =
  let u = fti todoItemUse i
      l =  fti todoItemLength i
      fti itf = fromInteger . toInteger . itf 
      f = case (todoItemEndtime i) of
        Nothing -> 1
        Just tend -> 
          let tl = tend `diffUTCTime` todoItemCreated i
              t1 = t `diffUTCTime` todoItemCreated i
              tfun = n' 0 1 . fromRational . toRational
          in tfun $ t1 / tl 
  in (u / l) * f
     
sortTodoList :: UTCTime -> [TodoItem] -> [TodoItem]    
sortTodoList t = sortBy $ \a b -> compare (todoItemV t a) (todoItemV t b)
     
n :: (Floating f) => f -> f -> f -> f
n u o x = 
  let e = exp $ ((-1) / 2) * ((x - u) / o) ^ 2
  in (1 / (o * sqrt (2 * pi))) * e

n' :: (Floating f) => f -> f -> f -> f
n' u o x =
  let nm = nmax o
  in n u o x / nm
     
nmax :: (Floating f) => f -> f
nmax o = 1 / (o * sqrt ( 2 * pi) )

