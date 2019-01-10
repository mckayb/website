{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Helpers.Markdown where

import Prelude
import Database.Persist.Sql (PersistField, PersistFieldSql)
import Data.Text (Text)
import Yesod.Form (FormMessage, Textarea(Textarea))
import Yesod.Form.Types (Field)
import Yesod.Core (HandlerSite, RenderMessage)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Text.MMark as MMark
import qualified Data.Text as Text
import qualified Yesod.Form as Form
import qualified Yesod.Form.Functions as FormFunctions


newtype Markdown =
  Markdown { unMarkdown ::  Text }
  deriving (Eq, Generic, PersistField, PersistFieldSql, Show)

instance FromJSON Markdown
instance ToJSON Markdown

mkMarkdown :: Textarea -> Either Text Markdown
mkMarkdown (Textarea str) = case MMark.parse "" str of
  Left errs -> (Left . Text.pack) $ MMark.parseErrorsPretty str errs
  Right _ -> (Right . Markdown) str

markdownField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Markdown
markdownField = FormFunctions.checkMMap (return . mkMarkdown) (Textarea . unMarkdown) Form.textareaField