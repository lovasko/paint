{- |
Module      : Text.Paint
Description : Colorization of command-line output
Copyright   : (c) 2017 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable
-}

module Text.Paint
( Color(..) -- *
, Flag(..)  -- *
, Paint(..) -- *
, paint     -- Paint -> Text -> Text
) where

import qualified Data.Text as T


-- | Foreground and background color.
data Color
  = Black        -- ^ black
  | Red          -- ^ red
  | Green        -- ^ green
  | Yellow       -- ^ yellow
  | Blue         -- ^ blue
  | Magenta      -- ^ magenta
  | Cyan         -- ^ cyan
  | LightGray    -- ^ light gray
  | DarkGray     -- ^ dark gray
  | LightRed     -- ^ light red
  | LightGreen   -- ^ light green
  | LightYellow  -- ^ light yellow
  | LightBlue    -- ^ light blue
  | LightMagenta -- ^ light magenta
  | LightCyan    -- ^ light cyan
  | White        -- ^ white
  | Default      -- ^ default setting
  deriving (Eq, Ord, Enum, Show)

-- | Various flags applicable to the text.
data Flag
  = Bold      -- ^ bold text (sometimes lighter)
  | Underline -- ^ underlined text
  | Blink     -- ^ blinking (<150 BPM)
  deriving (Eq, Ord, Show)

-- | Coloring scheme containing foreground and background layers, and a
-- setting for text styling.
data Paint = Paint Color Color [Flag]

-- | Convert a flag into an ANSI code.
flagCode :: Flag -- ^ flag
         -> Int  -- ^ ANSI code
flagCode Bold      = 1
flagCode Underline = 4
flagCode Blink     = 5

-- | Convert a foreground color into an ANSI code.
fgCode :: Color -- ^ color
       -> Int   -- ^ ANSI code
fgCode Black        = 30
fgCode Red          = 31
fgCode Green        = 32
fgCode Yellow       = 33
fgCode Blue         = 34
fgCode Magenta      = 35
fgCode Cyan         = 36
fgCode LightGray    = 37
fgCode Default      = 39
fgCode DarkGray     = 90
fgCode LightRed     = 91
fgCode LightGreen   = 92
fgCode LightYellow  = 93
fgCode LightBlue    = 94
fgCode LightMagenta = 95
fgCode LightCyan    = 96
fgCode White        = 97

-- | Convert a background color into an ANSI code.
bgCode :: Color -- ^ color
       -> Int   -- ^ ANSI code
bgCode Black        = 40
bgCode Red          = 41
bgCode Green        = 42
bgCode Yellow       = 43
bgCode Blue         = 44
bgCode Magenta      = 45
bgCode Cyan         = 46
bgCode LightGray    = 47
bgCode Default      = 49
bgCode DarkGray     = 100
bgCode LightRed     = 101
bgCode LightGreen   = 102
bgCode LightYellow  = 103
bgCode LightBlue    = 104
bgCode LightMagenta = 105
bgCode LightCyan    = 106
bgCode White        = 107

-- | Convert a coloring scheme into an ANSI escape sequence.
translate :: Paint  -- ^ coloring scheme
          -> T.Text -- ^ escape sequence
translate (Paint fg bg flags) = T.concat [T.pack "\x1b[", codes, T.pack "m"]
  where
    codes  = T.intercalate (T.singleton ';') $ map (T.pack . show) codes'
    codes' = map flagCode flags ++ [fgCode fg] ++ [bgCode bg]

-- | Apply a color scheme to a text instance.
paint :: Paint  -- ^ coloring scheme
      -> T.Text -- ^ plain text
      -> T.Text -- ^ colorized text
paint pnt text = T.concat [translate pnt, text, T.pack "\x1b[0m"]
