{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Helpers.Auth
import Helpers.Slug
import qualified Helpers.Theme as Theme

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings    :: AppSettings
  , appStatic      :: Static -- ^ Settings for static file serving.
  , appConnPool    :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger      :: Logger
  }

data MenuItem = MenuItem
  { menuItemLabel :: Text
  , menuItemRoute :: Route App
  , menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
    fromMaybe (getApprootText guessApproot app req) (appRoot $ appSettings app)

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ = Just <$> defaultClientSessionBackend
    120    -- timeout in minutes
    "config/client_session_key.aes"

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    -- mmsg <- getMessage
    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR css_bootstrap_css
      addStylesheet $ StaticR css_fontawesome_css
      addStylesheet $ StaticR css_brands_css
      -- Media queries don't seem to work in lucius
      -- So I had to put them in here instead
      addStylesheet $ StaticR css_app_css

      toWidget [julius|
        $("#sidebar-toggle").on("click", function() {
          $(".page__left").toggleClass("page__left--hidden")
          $("#sidebar-toggle").toggleClass("active")
        })
      |]
      toWidget [lucius|
        html {
          font-size: 16px;
        }

        html,body {
          margin: 0;
          height: 100%;
          background-color: #{Theme.mainColor Theme.colorScheme};
          color: #{Theme.textColor Theme.colorScheme};
          overflow-x: hidden;
        }

        .coordinates {
          display: flex;
        }

        .coordinates.coordinates--x {
          flex-direction: row;
        }

        .coordinates.coordinates--y {
          flex-direction: column;
        }

        .page {
          min-height: 100%;
        }

        .page a {
          color: #{Theme.linkColor Theme.colorScheme};
        }

        .page a:hover,
        .page a:focus {
          color: #{Theme.hoverColor Theme.colorScheme};
          text-decoration: none;
        }

        .page .page__left {
          background-color: #{Theme.sidebarColor Theme.colorScheme};
          color: #{Theme.textColor Theme.colorScheme};
          border-right: 1px solid #{Theme.borderColor Theme.colorScheme};
        }

        .page .page__left.page__left--hidden {
          display: none;
        }

        .page .page__right {
          min-width: 100%;
        }

        .page .page__header {
          padding: 2vh 2vw;
          background-color: #{Theme.headerColor Theme.colorScheme};
          border-bottom: 1px solid #{Theme.borderColor Theme.colorScheme};
        }

        .page .page__content {
          padding: 4vh 8vw;
          background-color: #{Theme.mainColor Theme.colorScheme};
        }

        .page .page__synopsis {
          padding: 20px;
        }

        .banner .banner__brand {
          font-size: 1.5rem;
          align-self: center;
          color: #{Theme.sidebarColor Theme.colorScheme};
        }

        .banner .banner__promos {
          flex: 1;
          text-align: right;
          align-self: center;
          font-size: 1.5rem;
        }

        .banner .banner__promos a {
          color: #{Theme.linkColor Theme.colorScheme};
          margin-left: 1vw;
        }

        .banner .banner__promos a:hover,
        .banner .banner__promos a:focus {
          text-decoration: none;
          color: #{Theme.hoverColor Theme.colorScheme};
        }

        .banner .banner__actions {
          margin-right: 2vw;
        }

        .banner .banner__actions > button,
        .banner .banner__actions > button.active:hover,
        .banner .banner__actions > button.active:focus {
          background-color: transparent;
          border-color: #{Theme.linkColor Theme.colorScheme};
          color: #{Theme.linkColor Theme.colorScheme};
          border-width: 1px;
        }

        .banner .banner__actions > button:not(.active):hover,
        .banner .banner__actions > button:not(.active):focus,
        .banner .banner__actions > button.active {
          background-color: #{Theme.hoverColor Theme.colorScheme};
          color: #{Theme.textColor Theme.colorScheme};
          border-color: #{Theme.hoverColor Theme.colorScheme};
        }

        .list {
          padding-left: 0;
        }

        .list .list__title {
          padding: 10px 15px;
          font-size: 1.10rem;
          border-bottom: 1px solid #{Theme.borderColor Theme.colorScheme};
        }

        .list .list__item {
          display: block;
          padding: 10px 15px;
          border-bottom: 1px solid #{Theme.borderColor Theme.colorScheme};
          color: #{Theme.linkColor Theme.colorScheme};
        }

        .list .list__item:first-child {
          border-top: 1px solid #{Theme.borderColor Theme.colorScheme};
        }

        .list .list__item i {
          padding-right: 1vw;
        }

        .list .list__item:hover,
        .list .list__item:focus {
          background-color: #{Theme.hoverColor Theme.colorScheme};
          color: #{Theme.textColor Theme.colorScheme};
        }

        .source-code pre {
          background-color: #{Theme.headerColor Theme.colorScheme};
          border: 1px solid #{Theme.borderColor Theme.colorScheme};
          color: #{Theme.textColor Theme.colorScheme};
        }
      |]
      [whamlet|
        <div .page.coordinates.coordinates--x>
          <div .page__left.page__left--hidden>
            <div .page__sidebar>
              <div .page__synopsis>This website is devoted to various rants about programming, math or whatever else I decide to monologue about.
              <div .list>
                <div .list__title>Social Media
                <a .list__item href="https://github.com/mckayb">
                  <i .fab.fa-lg.fa-github>
                  GitHub
                <a .list__item href="https://twitter.com/mckay_broderick">
                  <i .fab.fa-lg.fa-twitter>
                  Twitter
                <a .list__item href="https://www.linkedin.com/in/mckaybroderick/">
                  <i .fab.fa-lg.fa-linkedin>
                  LinkedIn
          <div .page__right.coordinates.coordinates--y>
            <div .page__header.banner.coordinates.coordinates--x>
              <div .banner__actions>
                <button .btn.btn-default #sidebar-toggle>
                  <i .glyphicon.glyphicon-menu-hamburger>
              <div .banner__brand>
                <a href=@{BlogR}>Structured Rants</a>
              <div .banner__promos>
                <a href="https://github.com/mckayb/website">
                  <i .fab.fa-lg.fa-github>
            <div .page__content>
              ^{widget}
      |]
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute
    :: App
    -> Maybe (Route App)
  authRoute _ = Just LoginR

  isAuthorized
    :: Route App  -- ^ The route the user is visiting.
    -> Bool       -- ^ Whether or not this is a "write" request.
    -> Handler AuthResult
  -- Routes not requiring authentication.
  isAuthorized RegisterR _ = return Authorized
  isAuthorized LoginR _ = return Authorized
  isAuthorized BlogR _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized (StaticR _) _ = return Authorized
  isAuthorized LivenessR _ = return Authorized
  isAuthorized ReadinessR _ = return Authorized
  isAuthorized (BlogPostSlugR _) _ = return Authorized
  isAuthorized RssR _ = return Authorized

  -- Routes requiring authentication delegate to
  -- the isAuthenticated function
  isAuthorized PostR _ = isAuthenticatedAdmin
  isAuthorized RoleR _ = isAuthenticatedAdmin
  isAuthorized TagR _ = isAuthenticatedAdmin

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent
    :: Text  -- ^ The file extension
    -> Text -- ^ The MIME content type
    -> LByteString -- ^ The contents of the file
    -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
    -- Generate a unique filename based on the content itself
    genFileName lbs = "autogen-" ++ base64md5 lbs

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return $
    appShouldLogAll (appSettings app)
      || level == LevelWarn
      || level == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  -- Takes the route that the user is currently on, and returns a tuple
  -- of the 'Text' that you want the label to display, and a previous
  -- breadcrumb route.
  breadcrumb
    :: Route App  -- ^ The route the user is visiting currently.
    -> Handler (Text, Maybe (Route App))
  breadcrumb BlogR = return ("Blog", Nothing)

  breadcrumb LoginR = return ("Login", Nothing)
  breadcrumb RegisterR = return ("Register", Nothing)

  breadcrumb _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
      master <- getYesod
      runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner appConnPool

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
