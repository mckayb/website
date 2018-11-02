{-# LANGUAGE QuasiQuotes #-}

module Handler.Checks where

import Import
import Text.Shakespeare.Text (st)
import Database.Persist.Sql (Single(unSingle), rawSql)

getLivenessR :: Handler ()
getLivenessR = return ()

getReadinessR :: Handler Value
getReadinessR = do
  result <- runDB isDatabaseUp
  returnJson result

isDatabaseUp :: DB [Text]
isDatabaseUp = do
  ready <- rawSql [st|
    SELECT 'READY'
  |] []
  return $ map unSingle ready