{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Helpers.Forms where

import Import

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

renderPanel :: Widget -> Widget
renderPanel widget = [whamlet|
<div .panel>
  <div .panel-body>
    ^{widget}
|]