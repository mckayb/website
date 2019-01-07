module Helpers.Database where

import Import
import Helpers.Email (Email)
import Helpers.Slug (Slug)
import Database.Esqueleto ((^.), (?.))
import qualified Database.Esqueleto as E
import qualified Data.Map.Strict as M

getUsers :: Handler [Entity User]
getUsers = runDB $ selectList [] []

getUserByEmail :: Email -> Handler (Maybe (Entity User))
getUserByEmail email = runDB $ selectFirst [UserEmail ==. email] []

getPasswordByUser :: Entity User -> Handler (Maybe (Entity Password))
getPasswordByUser user = runDB $ getBy $ UniquePasswordUser $ entityKey user

getPosts :: Handler [Entity Post]
getPosts = runDB $ selectList [] []

getPost :: Key Post -> Handler (Maybe (Entity Post))
getPost postId = runDB $ selectFirst [PostId ==. postId] []

getPostBySlug :: Slug -> Handler (Maybe (Entity Post))
getPostBySlug slug = runDB $ selectFirst [PostSlug ==. slug] []

groupFirst :: Ord a => [(a, Maybe b)] -> Map a [b]
groupFirst = foldr (\tuple acc -> M.insertWith (++) (fst tuple) (maybeToList (snd tuple)) acc) M.empty

getPostsWithTags :: Handler (Map (Entity Post) [Entity Tag])
getPostsWithTags = do
  postTagTuples <- runDB
    $ E.select
    $ E.from $ \(p `E.LeftOuterJoin` mpt `E.LeftOuterJoin` mt) -> do
    E.on (mpt ?. PostTagTagId E.==. mt ?. TagId)
    E.on (E.just (p ^. PostId) E.==. mpt ?. PostTagPostId)
    return (p, mt)
  return $ groupFirst postTagTuples

getRoles :: Handler [Entity Role]
getRoles = runDB $ selectList [] []

getRoleByUser :: Entity User -> Handler (Maybe Role)
getRoleByUser u = runDB $ get $ (userRoleId . entityVal) u

getRoleByName :: Text -> Handler (Maybe (Entity Role))
getRoleByName n = runDB $ selectFirst [RoleName ==. n] []

getTags :: Handler [Entity Tag]
getTags = runDB $ selectList [] []

getTagByName :: Text -> Handler (Maybe (Entity Tag))
getTagByName n = runDB $ selectFirst [TagName ==. n] []

insertUser :: User -> Handler (Key User)
insertUser user = runDB $ insert user

insertPassword :: Password -> Handler (Key Password)
insertPassword password = runDB $ insert password

insertPost :: Post -> Handler (Key Post)
insertPost post' = runDB $ insert post'