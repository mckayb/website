{-# LANGUAGE QuasiQuotes #-}

module Handler.Blog where

import Import
import qualified Database.Persist.Sql as Sql (fromSqlKey)
import qualified Helpers.Database as Database
import qualified Data.Text as Text
import qualified Helpers.Theme as Theme
import qualified Data.Map.Strict as Map
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension.Common as Ext
import qualified Lucid.Base as Lucid
import qualified Data.Text.Lazy as LazyText


getPostContent :: Entity Post -> Text
getPostContent post = case MMark.parse "" content of
  Left errs -> (Text.pack "<h2>Whoops...</h2>") <> (Text.pack $ MMark.parseErrorsPretty content errs)
  Right parsed -> render parsed
  where
    render = LazyText.toStrict . Lucid.renderText . MMark.render . MMark.useExtensions [Ext.skylighting]
    content = (postContent . entityVal) post

getTimestamp :: String -> Entity Post -> Text
getTimestamp fmt = Text.pack . formatTime defaultTimeLocale fmt . postTimestamp . entityVal

getPostTitle :: Entity Post -> Text
getPostTitle = postTitle . entityVal

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

takeUntilFirstParagraphInc :: [Text] -> [Text]
takeUntilFirstParagraphInc = takeWhileOneMore (not . Text.isPrefixOf "<p>")

getPostTeaser :: Entity Post -> Text
getPostTeaser = Text.unlines . takeUntilFirstParagraphInc . Text.lines . getPostContent

getBlogR :: Handler Html
getBlogR = do
  postTagsMap <- Database.getPostsWithTags
  let posts = sortBy (comparing $ Down . postTimestamp . entityVal) . Map.keys $ postTagsMap
  let tagsForPost post = sortOn (tagName . entityVal) $ postTagsMap Map.! post
  let getId = Sql.fromSqlKey . entityKey
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
        border-bottom: 1px solid #{Theme.borderColor Theme.colorScheme};
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
        <div .row>
          <div .col-md-12>Coming soon!

      $else
        $forall post <- posts
          <article .post.coordinates.coordinates--x>
            <div .post__sidebar.coordinates.coordinates--y>
              <div .post__date>
                <div .post__date_day>#{getTimestamp "%d" post}
                <div .post__date_mon_year>#{getTimestamp "%b %Y" post}
              <div .post__tags.coordinates.coordinates--y>
                $forall tag <- (tagsForPost post)
                  <div .post__tag.badge>#{(tagName . entityVal) tag}
            <div .post__body.coordinates.coordinates--y>
              <div .post__title>
                <a href="@{BlogR}post/#{getId post}">#{getPostTitle post}
              <div .post__content>
                #{preEscapedToMarkup (getPostTeaser post)}
    |]

getBlogPostR :: Key Post -> Handler Html
getBlogPostR postId = do
  post <- Database.getPost postId
  case post of
    Just post' -> defaultLayout $ do
      setTitle "Structured Rants"
      toWidget [lucius|
        .blog-post .blog-post__header {
          border-bottom: 1px solid #{Theme.sidebarColor Theme.colorScheme};
          margin-bottom: 5vh;
        }

        .source-code pre {
          background: #{Theme.headerColor Theme.colorScheme};
          border: 1px solid #{Theme.borderColor Theme.colorScheme};
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
    Nothing -> notFound