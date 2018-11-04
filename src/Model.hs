{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}

module Model where

import ClassyPrelude.Yesod
import Control.Monad.Logger (runNoLoggingT)
import Database.Persist.Quasi
import Helpers.BCrypt
import Helpers.Email
import Database.Persist.Sql (runSqlPersistMPool)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlPool, pgConnStr, runMigration, runMigrationUnsafe)
import Helpers.Types (DB)
import qualified Settings

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

appDBConn :: ConnectionString
appDBConn = pgConnStr $ Settings.appDatabaseConf Settings.compileTimeAppSettings

runAppMigrationsUnsafe :: DB ()
runAppMigrationsUnsafe = runMigrationUnsafe migrateAll

runAppMigrationsSafe :: DB ()
runAppMigrationsSafe = runMigration migrateAll

runAppDB :: DB a -> IO a
runAppDB a =
  runNoLoggingT $
    withPostgresqlPool appDBConn 3
      $ \pool -> liftIO $ runSqlPersistMPool a pool

seedInitialData :: DB ()
seedInitialData = do
  adminRole <- insertEntity $ Role "Admin"
  _ <- insertEntity $ Role "Commoner"
  let Just email = mkEmail "test@example.com"
  pass' <- liftIO $ hashPassword "password123"
  adminUser <- insertEntity $ User email (entityKey adminRole)
  _ <- insertEntity $ Password (entityKey adminUser) pass'
  return ()
