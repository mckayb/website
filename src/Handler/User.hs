module Handler.User where

import Import
import Helpers.Database

getUserR :: Handler Value
getUserR = do
  users <- getUsers
  returnJson users

postUserR :: Handler Value
postUserR = do
  user <- requireJsonBody :: Handler User
  insertedUser <- runDB $ insertEntity user
  returnJson insertedUser