{-# LANGUAGE OverloadedStrings #-}
-- | This is my Termonad configuration.
-- Currently defaults to Gruvbox

module Main where

import Data.Maybe (fromMaybe)
import Termonad
  ( CursorBlinkMode(CursorBlinkModeOn)
  , Option(Set)
  , ShowScrollbar(ShowScrollbarNever)
  , TMConfig
  , confirmExit
  , cursorBlinkMode
  , defaultConfigOptions
  , defaultTMConfig
  , options
  , showMenu
  , showScrollbar
  , start
  , FontConfig
  , FontSize(FontSizePoints)
  , defaultFontConfig
  , fontConfig
  , fontFamily
  , fontSize
  )
import Termonad.Config.Colour
  ( AlphaColour
  , ColourConfig
  , Palette(ExtendedPalette)
  , addColourExtension
  , createColour
  , createColourExtension
  , defaultColourConfig
  , defaultStandardColours
  , defaultLightColours
  , backgroundColour
  , foregroundColour
  , palette
  , List8
  , mkList8
  )

-- This is our main 'TMConfig'.  It holds all of the non-colour settings
-- for Termonad.
--
-- This shows how a few settings can be changed.
myTMConfig :: TMConfig
myTMConfig =
  defaultTMConfig
    { options =
        defaultConfigOptions
          { showScrollbar = ShowScrollbarNever
          , confirmExit = False
          , showMenu = False
          , cursorBlinkMode = CursorBlinkModeOn
          , fontConfig = fontConf
          }
    }

-- This is our Gruvbox dark 'ColourConfig'.  It holds all of our dark-related settings.
gruvboxDark :: ColourConfig (AlphaColour Double)
gruvboxDark =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (createColour 213 196 161) -- fg2
    , backgroundColour = Set (createColour  40  40  40) -- bg0
    -- Set the extended palette that has 2 Vecs of 8 Gruvbox palette colours
    , palette = ExtendedPalette gruvboxDark1 gruvboxDark2
    }
  where
    gruvboxDark1 :: List8 (AlphaColour Double)
    gruvboxDark1 = fromMaybe defaultStandardColours $ mkList8
      [ createColour  40  40  40 -- bg0
      , createColour 204  36  29 -- red.1
      , createColour 152 151  26 -- green.2
      , createColour 215 153  33 -- yellow.3
      , createColour  69 133 136 -- blue.4
      , createColour 177  98 134 -- purple.5
      , createColour 104 157 106 -- aqua.6
      , createColour 189 174 147 -- fg3
      ]

    gruvboxDark2 :: List8 (AlphaColour Double)
    gruvboxDark2 = fromMaybe defaultStandardColours $ mkList8
      [ createColour 124 111 100 -- bg4
      , createColour 251  71  52 -- red.9
      , createColour 184 187  38 -- green.10
      , createColour 250 189  47 -- yellow.11
      , createColour 131 165 152 -- blue.12
      , createColour 211 134 155 -- purple.13
      , createColour 142 192 124 -- aqua.14
      , createColour 235 219 178 -- fg1
      ]

-- This is our Gruvbox light 'ColourConfig'.  It holds all of our dark-related settings.
gruvboxLight :: ColourConfig (AlphaColour Double)
gruvboxLight =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (createColour  60  56  54) -- fg1
    , backgroundColour = Set (createColour 251 241 199) -- bg0
    -- Set the extended palette that has 2 Vecs of 8 Gruvbox palette colours
    , palette = ExtendedPalette gruvboxLight1 gruvboxLight2
    }
  where
    gruvboxLight1 :: List8 (AlphaColour Double)
    gruvboxLight1 = fromMaybe defaultStandardColours $ mkList8
      [ createColour 251 241 199 -- bg0
      , createColour 204  36  29 -- red.1
      , createColour 152 151  26 -- green.2
      , createColour 215 153  33 -- yellow.3
      , createColour  69 133 136 -- blue.4
      , createColour 177  98 134 -- purple.5
      , createColour 104 157 106 -- aqua.6
      , createColour 102  82  84 -- fg3
      ]

    gruvboxLight2 :: List8 (AlphaColour Double)
    gruvboxLight2 = fromMaybe defaultStandardColours $ mkList8
      [ createColour 168 153 132 -- bg4
      , createColour 157   0   6 -- red.9
      , createColour 121 116  14 -- green.10
      , createColour 181 118  20 -- yellow.11
      , createColour   7 102 120 -- blue.12
      , createColour 143  63 113 -- purple.13
      , createColour  66 123  88 -- aqua.14
      , createColour  60  56  54 -- fg1
      ]

-- This is our Solarized dark 'ColourConfig'.  It holds all of our dark-related settings.
solarizedDark :: ColourConfig (AlphaColour Double)
solarizedDark =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (createColour 131 148 150) -- base0
    , backgroundColour = Set (createColour   0  43  54) -- base03
    -- Set the extended palette that has 2 Vecs of 8 Solarized palette colours
    , palette = ExtendedPalette solarizedDark1 solarizedDark2
    }
  where
    solarizedDark1 :: List8 (AlphaColour Double)
    solarizedDark1 = fromMaybe defaultStandardColours $ mkList8
      [ createColour   7  54  66 -- base02
      , createColour 220  50  47 -- red
      , createColour 133 153   0 -- green
      , createColour 181 137   0 -- yellow
      , createColour  38 139 210 -- blue
      , createColour 211  54 130 -- magenta
      , createColour  42 161 152 -- cyan
      , createColour 238 232 213 -- base2
      ]

    solarizedDark2 :: List8 (AlphaColour Double)
    solarizedDark2 = fromMaybe defaultStandardColours $ mkList8
      [ createColour   0  43  54 -- base03
      , createColour 203  75  22 -- orange
      , createColour  88 110 117 -- base01
      , createColour 101 123 131 -- base00
      , createColour 131 148 150 -- base0
      , createColour 108 113 196 -- violet
      , createColour 147 161 161 -- base1
      , createColour 253 246 227 -- base3
      ]

-- This is our Solarized light 'ColourConfig'.  It holds all of our light-related settings.
solarizedLight :: ColourConfig (AlphaColour Double)
solarizedLight =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (createColour 101 123 131) -- base00
    , backgroundColour = Set (createColour 253 246 227) -- base3
    -- Set the extended palette that has 2 Vecs of 8 Solarized palette colours
    , palette = ExtendedPalette solarizedLight1 solarizedLight2
    }
  where
    solarizedLight1 :: List8 (AlphaColour Double)
    solarizedLight1 = fromMaybe defaultStandardColours $ mkList8
      [ createColour   7  54  66 -- base02
      , createColour 220  50  47 -- red
      , createColour 133 153   0 -- green
      , createColour 181 137   0 -- yellow
      , createColour  38 139 210 -- blue
      , createColour 211  54 130 -- magenta
      , createColour  42 161 152 -- cyan
      , createColour 238 232 213 -- base2
      ]

    solarizedLight2 :: List8 (AlphaColour Double)
    solarizedLight2 = fromMaybe defaultStandardColours $ mkList8
      [ createColour   0  43  54 -- base03
      , createColour 203  75  22 -- orange
      , createColour  88 110 117 -- base01
      , createColour 101 123 131 -- base00
      , createColour 131 148 150 -- base0
      , createColour 108 113 196 -- violet
      , createColour 147 161 161 -- base1
      , createColour 253 246 227 -- base3
      ]

-- This defines the font for the terminal.
fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "Droid Sans Mono Dotted for Powerline"
    , fontSize = FontSizePoints 6
    }

main :: IO ()
main = do
  -- First, create the colour extension based on either Solarized modules.
  myColourExt <- createColourExtension gruvboxDark

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
