{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Helpers.Email (Email(unEmail), mkEmail, emailField') where

import Prelude (Eq, Show, Maybe(Nothing, Just), Either(Left, Right), Monad, return, (.), ($))
import GHC.Generics (Generic)
import Data.Text (Text, pack)
import Database.Persist.Sql (PersistField, PersistFieldSql)
import Text.Email.Validate (validate)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson (FromJSON, ToJSON)
import Yesod.Form (FormMessage, emailField)
import Yesod.Form.Functions (checkMMap)
import Yesod.Form.Types (Field)
import Yesod.Core (HandlerSite, RenderMessage)

newtype Email = Email { unEmail :: Text }
  deriving (Eq, Show, Generic, PersistField, PersistFieldSql)

instance FromJSON Email
instance ToJSON Email

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither def m = case m of
  Just b -> Right b
  Nothing -> Left def

mkEmail :: Text -> Maybe Email
mkEmail em = case validate (encodeUtf8 em) of
  Left _ -> Nothing
  Right _ -> Just $ Email em

textToEitherEmail :: Text -> Either Text Email
textToEitherEmail = maybeToEither (pack "Invalid email") . mkEmail

emailField' :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Email
emailField' = checkMMap (return . textToEitherEmail) unEmail emailField
