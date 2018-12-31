module Helpers.Theme where

import Data.Text (Text)

data ColorScheme = ColorScheme
  { mainColor :: Text
  , mainAccentColor :: Text
  , textColor :: Text
  , sidebarColor :: Text
  , linkColor :: Text
  , hoverColor :: Text
  , headerColor :: Text
  , borderColor :: Text
  }

colorScheme :: ColorScheme
colorScheme = ColorScheme
  { mainColor = "#20262e"
  , mainAccentColor = "#161a1f"
  , textColor = "#dedede"
  , sidebarColor = "#1c2128"
  , linkColor = "#009b90"
  , hoverColor = "#006c64"
  , headerColor = "#1c2128"
  , borderColor = "#2d333b"
  }