{-# LANGUAGE NoImplicitPrelude #-}

module API.JWT where

import Import hiding (exp)
import Data.Time.Clock.POSIX
import Web.JWT

generateNewToken :: Handler JSON
generateNewToken = do
  yesod <- getYesod
  let settings = appSettings yesod
  let jwtSecret = secret $ appJWTSecret settings
  let jwtIssuer = appJWTIssuer settings
  let jwtExp = appJWTExpiration settings
  timeToExpiration <- liftIO $ ((+ jwtExp) . round . (* 1000)) <$> getPOSIXTime
  let jwtClaims = def { iss = stringOrURI jwtIssuer
                      , exp = numericDate (fromInteger timeToExpiration)
                      }
  let jwt = encodeSigned HS256 jwtSecret jwtClaims
  return jwt