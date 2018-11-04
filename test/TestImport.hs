{-# LANGUAGE QuasiQuotes #-}

module TestImport
  ( module TestImport
  , module X
  ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X hiding (LoginR)
import Yesod.Test            as X
import Yesod.Core            as X (SessionBackend, defaultClientSessionBackend, setSession)
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)
import Helpers.BCrypt
import Helpers.Email

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
  app <- getTestYesod
  liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
  app <- getTestYesod
  fakeHandlerGetLogger appLogger app handler

runYesodSpec :: YesodSpec App -> Spec
runYesodSpec run = do
  settings <- runIO $ loadYamlSettings
    ["config/test-settings.yml", "config/settings.yml"]
    []
    useEnv
  foundation <- runIO $ makeFoundation settings
  runIO $ wipeDB foundation
  yesodSpec foundation run

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
  settings <- loadYamlSettings
    ["config/test-settings.yml", "config/settings.yml"]
    []
    useEnv
  foundation <- makeFoundation settings
  wipeDB foundation
  logWare <- liftIO $ makeLogWare foundation
  return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
  tables <- getTables
  sqlBackend <- ask
  let esc = connEscapeName sqlBackend . DBName

  let escapedTables = map esc tables
      query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables

  let resetIncQueries = map (\t -> "ALTER SEQUENCE " <> esc (t <> "_id_seq") <> " RESTART WITH 1") tables

  rawExecute query []
  traverse_ (flip rawExecute $ []) resetIncQueries

getTables :: DB [Text]
getTables = do
  tables <- rawSql [st|
    SELECT table_name
    FROM information_schema.tables
    WHERE table_schema = 'public';
  |] []

  return $ map unSingle tables

createRole :: Text -> YesodExample App (Entity Role)
createRole name = do
  role <- runDB $ insertEntity $ Role name
  return role

createUser :: (Entity Role) -> Email -> YesodExample App (Entity User)
createUser role email = do
  user <- runDB $ insertEntity $ User email (entityKey role)
  return user

createPassword :: (Entity User) -> Text -> YesodExample App (Entity Password)
createPassword user pass = do
  hash' <- liftIO $ hashPassword pass
  password <- runDB $ insertEntity $ Password (entityKey user) hash'
  return password

createPost :: (Entity User) -> Text -> Text -> UTCTime -> YesodExample App (Entity Post)
createPost user title content timestamp = do
  postEntity <- runDB $ insertEntity $ Post title content timestamp (entityKey user)
  return postEntity
