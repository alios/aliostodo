{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleContexts, GADTs#-}

module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time.Clock
import Database.Persist.Quasi


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
