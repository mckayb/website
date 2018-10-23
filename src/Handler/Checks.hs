module Handler.Checks where

import Import
import Helpers.Database

getLivenessR :: Handler ()
getLivenessR = return ()

-- getReadinessR :: Handler Value
getReadinessR :: Handler Text
getReadinessR = do
  _ <- getRoles
  return "Running"