{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Helpers.Types where

import ClassyPrelude.Yesod 

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
  (MonadIO m) => ReaderT SqlBackend m a