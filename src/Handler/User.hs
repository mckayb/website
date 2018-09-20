module Handler.User where

import Import

getUserR :: Handler Value
getUserR = do
    returnJson ([] :: [User])

postUserR :: Handler Value
postUserR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    user <- (requireJsonBody :: Handler User)

    -- user' = user { userPassword }

    insertedUser <- runDB $ insertEntity user
    returnJson insertedUser