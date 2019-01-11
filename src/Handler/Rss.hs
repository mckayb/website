

module Handler.Rss where

import Import
import Yesod.RssFeed (RepRss, rssFeed)
import qualified Helpers.Markdown as Markdown
import qualified Helpers.Database as Database
import qualified Data.Text as Text
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension.Common as Ext
import qualified Lucid.Base as Lucid
import qualified Data.Text.Lazy as LazyText


getPostContent :: Entity Post -> Text
getPostContent post = case MMark.parse "" content of
  Left errs -> Text.pack "<h2>Whoops...</h2>" <> Text.pack (MMark.parseErrorsPretty content errs)
  Right parsed -> render parsed
  where
    render = LazyText.toStrict . Lucid.renderText . MMark.render . MMark.useExtensions [Ext.ghcSyntaxHighlighter, Ext.skylighting]
    content = (Markdown.unMarkdown . postContent . entityVal) post

postToFeedEntry :: Entity Post -> FeedEntry (Route App)
postToFeedEntry post = FeedEntry
  (BlogPostSlugR $ (postSlug . entityVal) post)
  ((postTimestamp . entityVal) post)
  ((postTitle . entityVal) post)
  (preEscapedToMarkup $ getPostContent post)
  Nothing

getRssR :: Handler RepRss
getRssR = do
  posts <- Database.getPosts
  time <- liftIO getCurrentTime
  rssFeed $ Feed
    "Test"
    RssR
    BlogR
    "Structured Rants"
    "Description"
    "en-us"
    time
    Nothing
    (fmap postToFeedEntry posts)