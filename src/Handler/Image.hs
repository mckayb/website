{-# LANGUAGE QuasiQuotes #-}

module Handler.Image where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Yesod.Form.Types (Enctype)
import Helpers.Forms (FormReaction, FormAlert(Danger, Success))
import qualified System.FilePath as FilePath
import qualified Data.Text as Text
import qualified Helpers.Session as Session
import qualified Helpers.Forms as Forms

getImageR :: Handler Html
getImageR = do
  _ <- Session.requireAdminUser
  (formWidget, enctype) <- generateFormPost imageForm
  renderImage (createImageForm enctype formWidget) []

postImageR :: Handler Html
postImageR = do
  _ <- Session.requireAdminUser
  ((result, formWidget), enctype) <- runFormPost imageForm
  case result of
    FormSuccess file -> do
      filename <- writeToServer file
      renderImage (createImageForm enctype formWidget) [(Success, "Successfully uploaded image: " <> Text.pack filename)]
    _ -> renderImage (createImageForm enctype formWidget) [(Danger, "Form failed validation")]

renderImage :: Widget -> [FormReaction] -> Handler Html
renderImage form reactions =
  defaultLayout $ do
    setTitle "Structured Rants - Upload Image"
    Forms.renderPanel "Upload Image" $ do
      [whamlet|
        <section .image-upload>
          $if not (null reactions)
            <div>
              ^{Forms.formReactionWidget reactions}
            <br>

          <div>
            ^{form}
      |]

imageForm :: Form FileInfo
imageForm = renderBootstrap3 BootstrapBasicForm $
  areq fileField fileSettings Nothing
  where
    fileSettings = FieldSettings
      { fsLabel = "Image"
      , fsId = Nothing
      , fsTooltip = Nothing
      , fsName = Nothing
      , fsAttrs = []
      }

createImageForm :: Enctype -> Widget -> Widget
createImageForm enc form = do
  [whamlet|
    <form method="POST" action="@{ImageR}" enctype=#{enc}>
      ^{form}
      <input .btn.btn-primary type="submit" name="action" value="Submit"/>
  |]

writeToServer :: FileInfo -> Handler FilePath
writeToServer file = do
  let filename = FilePath.takeFileName . unpack $ fileName file
      path' = imageFilePath filename
  liftIO $ fileMove file path'
  return filename

imageFilePath :: String -> FilePath
imageFilePath f = "static/images" </> f