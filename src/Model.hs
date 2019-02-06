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
import Helpers.Slug
import Helpers.Markdown
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

instance Ord Post where
  (Post _ _ _ timestamp _ _) `compare` (Post _ _ _ timestamp2 _ _) = timestamp `compare` timestamp2

appDBConn :: IO ConnectionString
appDBConn = pgConnStr . Settings.appDatabaseConf <$> Settings.compileTimeAppSettingsReadEnv

runAppMigrationsUnsafe :: DB ()
runAppMigrationsUnsafe = runMigrationUnsafe migrateAll

runAppMigrationsSafe :: DB ()
runAppMigrationsSafe = runMigration migrateAll

runAppDB :: DB a -> IO a
runAppDB a = do
  dbConn <- appDBConn
  runNoLoggingT $
    withPostgresqlPool dbConn 3
      $ \pool -> liftIO $ runSqlPersistMPool a pool

runAppSeedDB :: DB ()
runAppSeedDB = do
  settings <- liftIO Settings.compileTimeAppSettingsReadEnv
  let adminEmail = Settings.appAdminEmail settings
  let adminPassword = Settings.appAdminPassword settings

  roles <- selectList ([] :: [Filter Role]) []
  -- If we've already seeded the initial data, we don't need to do it again
  if not (null roles)
    then return ()
    else do
      adminRole <- insertEntity $ Role "Admin"
      _ <- insertEntity $ Role "Commoner"
      case mkEmail adminEmail of
        Just email -> do
          adminUser <- insertEntity $ User email (entityKey adminRole)
          passHash <- liftIO $ hashPassword adminPassword
          _ <- insertEntity $ Password (entityKey adminUser) passHash
          return ()
        Nothing -> error "The admin email that was given is not a valid email address. Please check your settings."
