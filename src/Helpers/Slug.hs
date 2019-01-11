{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Helpers.Slug (Slug(unSlug), mkSlug, slugField) where

import Prelude
import Data.Text (Text)
import Database.Persist.Sql (PersistField, PersistFieldSql)
import Yesod.Core.Dispatch (PathPiece)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Yesod.Form (FormMessage)
import Yesod.Form.Types (Field)
import Yesod.Core (HandlerSite, RenderMessage)
import qualified Yesod.Form as Form
import qualified Yesod.Form.Functions as FormFunctions
import qualified Data.Text as Text
import qualified Data.Char as Char

newtype Slug = Slug {unSlug :: Text}
  deriving (Show, Ord, Read, Eq, Generic, PathPiece, PersistField, PersistFieldSql)

instance FromJSON Slug
instance ToJSON Slug

mkSlug :: Text -> Either Text Slug
mkSlug x = if Text.length x' > 0 then (Right . Slug) x' else Left "Invalid slug"
  where
    f = Text.filter (\c -> Char.isAlphaNum c || c == '-')
    x' = (Text.intercalate "-" . fmap f . Text.words . Text.toLower) x

slugField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Slug
slugField = FormFunctions.checkMMap (return . mkSlug) unSlug Form.textField