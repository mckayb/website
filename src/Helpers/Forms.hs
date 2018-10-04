{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Helpers.Forms where

import Import  

formErrorWidget :: [Text] -> Widget
formErrorWidget formErrors = [whamlet|
$if not (null formErrors)
  $forall formError <- formErrors
    <div .alert.alert-danger>
      #{formError}
|]

renderPanel :: Widget -> Widget
renderPanel widget = [whamlet|
<div .panel>
  <div .panel-body>
    ^{widget}
|]