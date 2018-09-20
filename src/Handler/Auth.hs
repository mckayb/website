{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Auth where

import Import

getLoginR :: Handler Html
getLoginR = do
    defaultLayout $ do
        setTitle "Login"
        $(widgetFile "login")
