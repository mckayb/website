{-# LANGUAGE QuasiQuotes #-}

module Helpers.Forms where

import Import
import qualified Helpers.Theme as Theme

data FormAlert = Success | Info | Warning | Danger
  deriving Eq

instance Show FormAlert where
  show Success = "success"
  show Info = "info"
  show Warning = "warning"
  show Danger = "danger"

type FormReaction = (FormAlert, Text)

formReactionWidget :: [FormReaction] -> Widget
formReactionWidget formReactions = [whamlet|
$if not (null formReactions)
  $forall formReaction <- formReactions
    <div .alert .alert-#{show $ fst formReaction}>
      #{snd formReaction}
|]

renderPanel :: Text -> Widget -> Widget
renderPanel title widget = do
  toWidget [lucius|
    .panel.panel--dark > .panel-heading {
      background-color: #{Theme.sidebarColor Theme.colorScheme};
      border-color: #{Theme.hoverColor Theme.colorScheme};
      color: white;
    }
  |]
  [whamlet|
    <div .panel.panel-default.panel--dark>
      <div .panel-heading>
        #{title}
      <div .panel-body>
        ^{widget}
  |]