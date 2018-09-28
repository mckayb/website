{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Auth.Login where

import Import hiding (exp)
import Web.JWT
import API.Database
import API.BCrypt
import Handler.Auth.Forms
import Data.Time.Clock.POSIX
import Database.Persist.Sql (fromSqlKey)
import qualified Data.Text as T (pack)

getLoginR :: Handler Html
getLoginR = do
  (formWidget, _) <- generateFormPost loginForm
  renderLogin formWidget []

postLoginR :: Handler Html
postLoginR = do
  ((result, formWidget), _) <- runFormPost loginForm
  -- Validate the Form
  case result of
    FormSuccess (email, password) -> do
      mUser <- getUserByEmail email
      -- Validate that there's a user with that email
      case mUser of
        Just user' -> do
          -- Validate that there's a password associated with that user
          mPassword <- getPasswordByUser user'
          case mPassword of
            Just (Entity _ password') -> do
              -- Validate that the password we have matches the password they gave
              let matches = passwordMatches (passwordHash password') password
              case matches of
                False -> renderLogin formWidget ["Incorrect username or password"]
                True -> do
                  -- Grab the JWT Secret and use that to sign a jwt
                  -- TODO: Move this somewhere else. Feels wrong to create it here
                  yesod <- getYesod
                  let settings = appSettings yesod
                  let jwtSecret = secret $ appJWTSecret settings
                  let jwtIssuer = appJWTIssuer settings
                  let jwtExp = appJWTExpiration settings
                  timeToExpiration <- liftIO $ ((+ jwtExp) . round . (* 1000)) <$> getPOSIXTime
                  let jwtClaims = def { iss = stringOrURI jwtIssuer
                                      , exp = numericDate (fromInteger timeToExpiration)
                                      }
                  let jwt = encodeSigned HS256 jwtSecret jwtClaims
                  let userIdText = T.pack . show $ fromSqlKey $ entityKey user'
                  setSession userIdText jwt
                  redirect HomeR
            Nothing -> do
              renderLogin formWidget ["Incorrect username or password"]
        Nothing -> do
          renderLogin formWidget ["Incorrect username or password"]
    _ -> do
      renderLogin formWidget ["Form failed validation"]

loginForm :: Form (Text, Text)
loginForm = renderDivs $ (,)
  <$> areq emailField emailSettings Nothing
  <*> areq passwordField passwordSettings Nothing
  where
    emailSettings = FieldSettings
      { fsLabel = "Email"
      , fsId = Nothing
      , fsTooltip = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "form-control"), ("placeholder", "Email")]
      }
    passwordSettings = FieldSettings
      { fsLabel = "Password"
      , fsId = Nothing
      , fsTooltip = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "form-control"), ("placeholder", "Password")]
      }

renderLogin :: Widget -> [Text] -> Handler Html
renderLogin widget errors =
  defaultLayout $ do
    setTitle "Login"
    renderPanel $ [whamlet|
      <div>
        ^{formErrorWidget errors}
      <div>
        <form method="POST" action="@{LoginR}">
          ^{widget}
          <input .btn.btn-primary type="submit" value="Login">
    |]



