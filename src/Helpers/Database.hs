{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Helpers.Database where

import Import
import Database.Persist.Sql

getUsers :: Handler [Entity User]
getUsers = runDB $ selectList [] []

getUserByEmail :: Text -> Handler (Maybe (Entity User))
getUserByEmail email = runDB $ selectFirst [UserEmail ==. email] []

getPasswordByUser :: Entity User -> Handler (Maybe (Entity Password))
getPasswordByUser user = runDB $ getBy $ UniquePasswordUser $ entityKey user

getPosts :: Handler [Entity Post]
getPosts = runDB $ selectList [] []

getPost :: Int64 -> Handler (Maybe (Entity Post))
getPost postId = runDB $ selectFirst [PostId ==. (toSqlKey postId)] []

getRoleByUser :: Entity User -> Handler (Maybe Role)
getRoleByUser u = runDB $ get $ (userRoleId . entityVal) u

insertUser :: User -> Handler (Key User)
insertUser user = runDB $ insert user

insertPassword :: Password -> Handler (Key Password)
insertPassword password = runDB $ insert password

insertPost :: Post -> Handler (Key Post)
insertPost post = runDB $ insert post