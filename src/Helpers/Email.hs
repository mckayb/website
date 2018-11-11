{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Helpers.Email (Email(unEmail), mkEmail, emailField') where

import Prelude
import GHC.Generics (Generic)
import Data.Text (Text)
import Database.Persist.Sql (PersistField, PersistFieldSql)
import Data.Aeson (FromJSON, ToJSON)
import Yesod.Form (FormMessage)
import Yesod.Form.Types (Field)
import Yesod.Core (HandlerSite, RenderMessage)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Text.Email.Validate as Validate
import qualified Yesod.Form as Form
import qualified Yesod.Form.Functions as FormFunctions

newtype Email = Email { unEmail :: Text }
  deriving (Eq, Show, Generic, PersistField, PersistFieldSql)

instance FromJSON Email
instance ToJSON Email

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither def m = case m of
  Just b -> Right b
  Nothing -> Left def

mkEmail :: Text -> Maybe Email
mkEmail em = case Validate.validate (Encoding.encodeUtf8 em) of
  Left _ -> Nothing
  Right _ -> Just $ Email em

textToEitherEmail :: Text -> Either Text Email
textToEitherEmail = maybeToEither (Text.pack "Invalid email") . mkEmail

emailField' :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Email
emailField' = FormFunctions.checkMMap (return . textToEitherEmail) unEmail Form.emailField
