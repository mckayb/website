{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module TestImport
  ( module TestImport
  , module X
  ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawSql, rawExecute, connEscapeName, unSingle)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X hiding (LoginR)
import Yesod.Test            as X
import Yesod.Core            as X (Yesod, Route, RedirectUrl, SessionBackend, defaultClientSessionBackend, setSession)
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)
import Text.Shakespeare.Text (st)
import Helpers.BCrypt
import Helpers.Email
import Helpers.Types
import Helpers.Slug
import Helpers.Markdown

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

getTables :: DB [Text]
getTables = do
  tables <- rawSql [st|
    SELECT table_name
    FROM information_schema.tables
    WHERE table_schema = 'public';
  |] []

  return $ map unSingle tables

truncateTables :: DB ()
truncateTables = do
  tables <- getTables
  sqlBackend <- ask
  let esc = connEscapeName sqlBackend . DBName

  let escapedTables = map esc tables
      query = [st|TRUNCATE TABLE #{intercalate ", " escapedTables} RESTART IDENTITY CASCADE|] 

  case escapedTables of
    [] -> error "Error: No tables to truncate!"
    _ -> rawExecute query []

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app truncateTables

createTag :: Text -> YesodExample App (Entity Tag)
createTag name =
  runDB $ insertEntity $ Tag name

createRole :: Text -> YesodExample App (Entity Role)
createRole name =
  runDB $ insertEntity $ Role name

createUser :: Entity Role -> Email -> YesodExample App (Entity User)
createUser role email =
  runDB $ insertEntity $ User email (entityKey role)

createPassword :: Entity User -> Text -> YesodExample App (Entity Password)
createPassword user pass = do
  hash' <- liftIO $ hashPassword pass
  runDB $ insertEntity $ Password (entityKey user) hash'

createPost :: Entity User -> Text -> Markdown -> Slug -> UTCTime -> Bool -> YesodExample App (Entity Post)
createPost user title content slug timestamp published =
  runDB $ insertEntity $ Post title content slug timestamp (entityKey user) published

createPostTag :: Entity Post -> Entity Tag -> YesodExample App (Entity PostTag)
createPostTag post' tag =
  runDB $ insertEntity $ PostTag (entityKey post') (entityKey tag)

just :: Maybe a -> a
just = fromMaybe (error "Was a Nothing!")

authGet :: ( Yesod site
           , RedirectUrl site url
           , RedirectUrl site (Route App)
           ) => Email
           -> Text
           -> url
           -> SIO (YesodExampleData site) ()
authGet em pass route = do
  get LoginR
  statusIs 200

  request $ do
    addToken
    byLabelExact "Email" $ unEmail em
    byLabelExact "Password" pass
    setMethod "POST"
    setUrl LoginR

  statusIs 303
  _ <- followRedirect
  statusIs 200
  get route
