{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module API.Database where

import Import

getUserByEmail :: Text -> Handler (Maybe (Entity User))
getUserByEmail email = runDB $ selectFirst [UserEmail ==. email] []

getPasswordByUser :: Entity User -> Handler (Maybe (Entity Password))
getPasswordByUser user = runDB $ getBy $ UniquePasswordUser $ entityKey user