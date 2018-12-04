{-# LANGUAGE OverloadedStrings #-}

-- | This is my configuration file for the Termonad terminal emulator.

module Main where

import Data.Colour.SRGB (Colour, sRGB24)
-- import Data.Colour (withOpacity)
import Termonad.App (defaultMain)
import Termonad.Config
  ( FontConfig, FontSize(FontSizePoints), Option(Set)
  , ShowScrollbar(ShowScrollbarNever), defaultFontConfig, defaultTMConfig
  , fontConfig, fontFamily, fontSize, showScrollbar, showMenu
  )
import Termonad.Config.Colour
import Termonad.Config.Vec (Vec, VecT((:+), EmptyV), N8)
import Termonad.Config.Extension ((<+>))

-- | This sets the color of the cursor in the terminal.
--
-- This uses the "Data.Colour" module to define a dark-red color.
-- There are many default colors defined in "Data.Colour.Names".
cursBgColor :: Colour Double
cursBgColor = sRGB24 204 0 0

-- | This sets the colors used for the terminal.  We only specify the background
-- color of the cursor.
colConf :: ColourConfig (Colour Double)
colConf =
  defaultColourConfig
    { cursorBgColour = Set cursBgColor -- `withOpacity` 0.7
    , palette = BasicPalette mySolarizedColours
    } where
        mySolarizedColours :: Vec N8 (Colour Double)
        mySolarizedColours
          =  sRGB24 0 43 54 -- base03, background
          :+ sRGB24 220 50 47 -- red
          :+ sRGB24 7 54 66 -- base02
          :+ sRGB24 203 75 22 -- orange
          :+ sRGB24 38 139 210 -- blue
          :+ sRGB24 211 54 130 -- magenta
          :+ sRGB24 42 161 152 -- cyan
          :+ sRGB24 211 54 130 -- magenta
          -- :+ sRGB24 211 54 130 -- magenta
          -- :+ sRGB24 211 54 130 -- magenta
          -- :+ sRGB24 211 54 130 -- magenta
          -- :+ sRGB24 211 54 130 -- magenta
          -- :+ sRGB24 211 54 130 -- magenta
          -- :+ sRGB24 211 54 130 -- magenta
          -- :+ sRGB24 211 54 130 -- magenta
          -- :+ sRGB24 211 54 130 -- magenta
          :+ EmptyV

-- | This defines the font for the terminal.
fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "DejaVu Sans Mono"
    , fontSize = FontSizePoints 8
    }

main :: IO ()
main = do
  let termonadConf =
        defaultTMConfig
          { fontConfig = fontConf
          -- Make sure the scrollbar is never visible.
          , showScrollbar = ShowScrollbarNever
          , showMenu = False
          } <+> colConf
  defaultMain termonadConf
