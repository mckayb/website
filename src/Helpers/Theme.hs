module Helpers.Theme where

import Data.Text (Text)

data ColorScheme = ColorScheme
  { mainColor :: Text
  , textColor :: Text
  , sidebarColor :: Text
  , linkColor :: Text
  , hoverColor :: Text
  , headerColor :: Text
  }

colorScheme :: ColorScheme
colorScheme = ColorScheme "#1F2833" "#B4BCC2" "#45A29E" "#45A29E" "#367E7B" "#1F2833"
