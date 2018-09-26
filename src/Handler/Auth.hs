{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Auth where

import Import

getLoginR :: Handler Html
getLoginR = do
    (formWidget, _) <- generateFormPost loginForm
    renderLogin formWidget []

postLoginR :: Handler Html
postLoginR = do
    ((result, formWidget), _) <- runFormPost loginForm
    liftIO $ print result
    case result of
        FormSuccess (_, _) -> do
            -- Create a JWT and store it in the session
            -- To validate that we're logged in, we'll just
            -- grab that from the session, decode and verify it
            -- and be on our merry way
            -- let jwt = encodeSigned signer claims
            -- setSession "jwt" jwt
            redirect HomeR
        _ -> do
            renderLogin formWidget ["Form failed validation"]

loginForm :: Form (Text, Text)
loginForm = renderDivs $ (,)
    <$> areq emailField "Email" Nothing
    <*> areq passwordField "Password" Nothing

formErrorWidget :: [Text] -> Widget
formErrorWidget formErrors = [whamlet|
$if not (null formErrors)
    $forall formError <- formErrors
        <div .alert.alert-danger>
            #{formError}
|]

renderPanel :: Widget -> Widget
renderPanel widget = [whamlet|
<div .panel>
    <div .panel-body>
        ^{widget}
|]

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



