module Helpers.Database where

import Import
import Helpers.Email (Email)
import Helpers.Slug (Slug)
import Database.Esqueleto ((^.), (?.))
import qualified Database.Esqueleto as E
import qualified Data.Map.Strict as M

getUserByEmail :: Email -> Handler (Maybe (Entity User))
getUserByEmail email = runDB $ selectFirst [UserEmail ==. email] []

getPasswordByUser :: Entity User -> Handler (Maybe (Entity Password))
getPasswordByUser user = runDB $ getBy $ UniquePasswordUser $ entityKey user

getPost :: Key Post -> Handler (Maybe (Entity Post))
getPost postId = runDB $ selectFirst [PostId ==. postId] []

getPosts :: Handler [Entity Post]
getPosts = runDB $ selectList [] []

getPostBySlug :: Slug -> Handler (Maybe (Entity Post))
getPostBySlug slug = runDB $ selectFirst [PostSlug ==. slug] []

getPostWithTags :: Key Post -> Handler (Maybe (Entity Post, [Entity Tag]))
getPostWithTags postId = do
  post <- getPost postId
  case post of
    Just post' -> do
      tags <- getTagsByPost post'
      return $ Just (post', tags)
    Nothing -> return Nothing

getPostsWithTags :: Bool -> Handler (Map (Entity Post) [Entity Tag])
getPostsWithTags published = do
  postTagTuples <- runDB
    $ E.select
    $ E.from $ \(p `E.LeftOuterJoin` mpt `E.LeftOuterJoin` mt) -> do
    E.on (mpt ?. PostTagTagId E.==. mt ?. TagId)
    E.on (E.just (p ^. PostId) E.==. mpt ?. PostTagPostId)
    E.where_ (p ^. PostPublished E.==. E.val published)
    return (p, mt)
  return $ groupFirst postTagTuples
  where
    groupFirst :: Ord a => [(a, Maybe b)] -> Map a [b]
    groupFirst = foldr (\tuple acc -> M.insertWith (++) (fst tuple) (maybeToList (snd tuple)) acc) M.empty

getRoleByUser :: Entity User -> Handler (Maybe (Entity Role))
getRoleByUser u = runDB $ selectFirst [RoleId ==. (userRoleId . entityVal) u] []

getRoleByName :: Text -> Handler (Maybe (Entity Role))
getRoleByName n = runDB $ selectFirst [RoleName ==. n] []

getTags :: Handler [Entity Tag]
getTags = runDB $ selectList [] []

getTagByName :: Text -> Handler (Maybe (Entity Tag))
getTagByName n = runDB $ selectFirst [TagName ==. n] []

getTagsByPost :: Entity Post -> Handler [Entity Tag]
getTagsByPost post = runDB
  $ E.select
  $ E.from $ \(pt `E.InnerJoin` t) -> do
  E.on ((pt ^. PostTagTagId) E.==. (t ^. TagId))
  E.where_ (pt ^. PostTagPostId E.==. E.val (entityKey post))
  return t

insertUser :: User -> Handler (Key User)
insertUser user = runDB $ insert user

insertPassword :: Password -> Handler (Key Password)
insertPassword password = runDB $ insert password