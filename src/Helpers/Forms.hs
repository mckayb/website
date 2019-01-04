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
    .panel.panel--themed {
      background-color: #{Theme.sidebarColor Theme.colorScheme};
      border-color: #{Theme.borderColor Theme.colorScheme};
    }

    .panel.panel--themed > .panel-body input,
    .panel.panel--themed > .panel-body select,
    .panel.panel--themed > .panel-body textarea {
      background: transparent;
      border-color: #{Theme.hoverColor Theme.colorScheme};
      color: white;
    }

    .panel.panel--themed > .panel-body input.btn:hover {
      background: #{Theme.hoverColor Theme.colorScheme}
    }

    .panel.panel--themed > .panel-heading {
      color: white;
      border-bottom: 1px solid #{Theme.borderColor Theme.colorScheme};
      font-size: 1.10rem;
    }
  |]
  [whamlet|
    <div .panel.panel--themed>
      <div .panel-heading>
        #{title}
      <div .panel-body>
        ^{widget}
  |]