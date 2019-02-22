

module Handler.Rss where

import Import
import Yesod.RssFeed (RepRss, rssFeed)
import qualified Helpers.Markdown as Markdown
import qualified Helpers.Database as Database


postToFeedEntry :: Entity Post -> FeedEntry (Route App)
postToFeedEntry post =
  FeedEntry
  { feedEntryLink = BlogPostSlugR $ postSlug post'
  , feedEntryUpdated = postTimestamp post'
  , feedEntryTitle = postTitle post'
  , feedEntryContent = preEscapedToMarkup $ Markdown.parseMarkdown (postContent post')
  , feedEntryEnclosure = Nothing
  }
  where post' = entityVal post

getRssR :: Handler RepRss
getRssR = do
  time <- liftIO getCurrentTime
  posts <- Database.getPublishedPosts
  rssFeed $ Feed
    { feedTitle = "Structured Rants"
    , feedLinkSelf = RssR
    , feedLinkHome = BlogR
    , feedAuthor = "McKay Broderick"
    , feedDescription = "Musings about Tech, Math, Arcade Games or whatever else I decide to monologue about."
    , feedLanguage = "en-us"
    , feedUpdated = time -- Change this to be the most recent post... maybe?
    , feedLogo = Just (FaviconR, "Structured Rants")
    , feedEntries = fmap postToFeedEntry posts
    }