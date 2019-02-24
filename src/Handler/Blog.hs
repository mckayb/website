{-# LANGUAGE QuasiQuotes #-}

module Handler.Blog where

import Import
import Helpers.Slug (Slug)
import qualified Helpers.Session as Session
import qualified Helpers.Database as Database
import qualified Helpers.Markdown as Markdown
import qualified Data.Text as Text
import qualified Helpers.Theme as Theme
import qualified Data.Map.Strict as Map

getPostContent :: Post -> Text
getPostContent = Markdown.parseMarkdown . postContent

getTimestamp :: String -> Post -> Text
getTimestamp fmt = Text.pack . formatTime defaultTimeLocale fmt . postTimestamp

getPostTitle :: Post -> Text
getPostTitle = postTitle

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

takeUntilFirstParagraphInc :: [Text] -> [Text]
takeUntilFirstParagraphInc = takeWhileOneMore (not . Text.isPrefixOf "<p>")

getPostTeaser :: Post -> Text
getPostTeaser = Text.unlines . takeUntilFirstParagraphInc . Text.lines . getPostContent

getPostSlug :: Post -> Slug
getPostSlug = postSlug

getBlogR :: Handler Html
getBlogR = do
  postTagsMap <- Database.getPostsWithTags True
  showPosts comingSoonWidget postTagsMap

getBlogDraftsR :: Handler Html
getBlogDraftsR = do
  postTagsMap <- Database.getPostsWithTags False
  showPosts noDraftsWidget postTagsMap

showPosts :: Widget -> Map (Entity Post) [Entity Tag] -> Handler Html
showPosts noPostsWidget postTagsMap = do
  isAdmin <- Session.isAdmin
  let posts = sortOn (Down . postTimestamp . entityVal) . Map.keys $ postTagsMap
  let tagsForPost post = sortOn (tagName . entityVal) $ postTagsMap Map.! post
  defaultLayout $ do
    setTitle "Structured Rants"
    toWidget [lucius|
      .post {
        margin-bottom: 2vh;
        background-color: #{Theme.headerColor Theme.colorScheme};
        border-radius: 5px;
        border: 1px solid #{Theme.borderColor Theme.colorScheme};
      }

      .post .post__content {
        padding: 0.5rem;
      }

      .post .post__actions {
        padding: 0.25rem;
      }

      .post .post__sidebar {
        text-align: center;
        padding: 0.5rem;
        border-right: 1px solid #{Theme.borderColor Theme.colorScheme};
      }

      .post .post__date_day {
        font-size: 1.25rem;
      }

      .post .post__header {
        border-bottom: 1px solid #{Theme.borderColor Theme.colorScheme};
      }

      .post .post__title {
        padding: 0.5rem;
        font-size: 1.25rem;
        flex: 1;
      }

      .post .post__actions {
        align-self: center;
      }

      .post h1,
      .post h2,
      .post h3 {
        margin-top: 0;
      }

      .post h1 {
        font-size: 24px;
      }

      .post h2 {
        font-size: 21px;
      }

      .post .post__left {
        border-right: 1px solid #{Theme.borderColor Theme.colorScheme};
      }

      .post .post__body {
        flex: 1;
      }

      .post .post__tag {
        margin-top: 5px;
      }

      .post .post__tag:first-child {
        margin-top: 15px;
      }
    |]
    [whamlet|
      $if (null posts)
        ^{noPostsWidget}

      $else
        $forall post <- posts
          <article .post.coordinates.coordinates--x>
            <div .post__sidebar.coordinates.coordinates--y>
              <div .post__date>
                <div .post__date_day>#{getTimestamp "%d" (entityVal post)}
                <div .post__date_mon_year>#{getTimestamp "%b %Y" (entityVal post)}
              <div .post__tags.coordinates.coordinates--y>
                $forall tag <- (tagsForPost post)
                  <div .post__tag.badge>#{(tagName . entityVal) tag}
            <div .post__body.coordinates.coordinates--y>
              <div .post__header.coordinates.coordinates--x>
                <div .post__title>
                  <a href="@{BlogPostSlugR (getPostSlug (entityVal post))}">#{getPostTitle (entityVal post)}
                $if isAdmin
                  <div .post__actions>
                    <a href="@{EditPostR (entityKey post)}">
                      <i .fa.fa-edit>
              <div .post__content>
                #{preEscapedToMarkup (getPostTeaser (entityVal post))}
    |]

getBlogPostSlugR :: Slug -> Handler Html
getBlogPostSlugR slug = do
  mPost <- Database.getPostBySlug slug
  case mPost of
    Just post' ->
      if (not . postPublished . entityVal) post'
        then Session.requireAdminUser >>= const ((render . entityVal) post')
        else (render.entityVal) post'
    Nothing -> notFound
  where
    render post' = defaultLayout $ do
      setTitle "Structured Rants"
      postWidget post'

noDraftsWidget :: Widget
noDraftsWidget = [whamlet|
  <div .row>
    <div .col-md-12>No drafts.
|]

comingSoonWidget :: Widget
comingSoonWidget = [whamlet|
  <div .row>
    <div .col-md-12>Coming soon!
|]

postWidget :: Post -> Widget
postWidget post' = do
  toWidget [lucius|
    .blog-post .blog-post__header {
      border-bottom: 1px solid #{Theme.borderColor Theme.colorScheme};
      margin-bottom: 3vh;
      padding-bottom: 5px;
    }

    .blog-post .blog-post__body h1,
    .blog-post .blog-post__body h2,
    .blog-post .blog-post__body h3,
    .blog-post .blog-post__body h4,
    .blog-post .blog-post__body h5,
    .blog-post .blog-post__body h6 {
      font-weight: bold;
      border-bottom: 1px solid #{Theme.borderColor Theme.colorScheme};
      padding-bottom: 5px;
    }
  |]
  [whamlet|
    <article .blog-post>
      <section .blog-post__header>
        <h1>#{getPostTitle post'}
        <div .blog-post__time .text-muted>#{getTimestamp "%d %B %Y" post'}
      <section .blog-post__body>
        #{preEscapedToMarkup (getPostContent post')}
  |]
