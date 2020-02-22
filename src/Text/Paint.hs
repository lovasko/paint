{- |
Module      : Text.Paint
Description : Colorization of command-line output
Copyright   : (c) 2017-2020 Daniel Lovasko
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
  = Black   -- ^ black
  | Maroon  -- ^ maroon 
  | Green   -- ^ green
  | Olive   -- ^ olive 
  | Navy    -- ^ navy 
  | Purple  -- ^ purple 
  | Teal    -- ^ teal 
  | Silver  -- ^ silver
  | Gray    -- ^ gray
  | Red     -- ^ red
  | Lime    -- ^ lime
  | Yellow  -- ^ yellow
  | Blue    -- ^ blue
  | Fuchsia -- ^ fuchsia
  | Aqua    -- ^ aqua
  | White   -- ^ white
  | Default -- ^ default
  deriving (Eq, Ord, Enum, Show)

-- | Various flags applicable to the text.
data Flag
  = Bold      -- ^ bold text (sometimes lighter)
  | Underline -- ^ underlined text
  | Blink     -- ^ blinking (<150 BPM)
  deriving (Eq, Ord, Show)

-- | Coloring scheme containing foreground and background layers, and a
-- setting for text styling.
data Paint
  = Paint Color Color [Flag]
  deriving (Eq, Ord, Show)

-- | Convert a flag into an ANSI code.
flagCode
  :: Flag -- ^ flag
  -> Int  -- ^ ANSI code
flagCode Bold      = 1
flagCode Underline = 4
flagCode Blink     = 5

-- | Convert a foreground color into an ANSI code.
fgCode
  :: Color -- ^ color
  -> Int   -- ^ ANSI code
fgCode Black   = 30
fgCode Maroon  = 31
fgCode Green   = 32
fgCode Olive   = 33
fgCode Navy    = 34
fgCode Purple  = 35
fgCode Teal    = 36
fgCode Silver  = 37
fgCode Default = 39
fgCode Gray    = 90
fgCode Red     = 91
fgCode Lime    = 92
fgCode Yellow  = 93
fgCode Blue    = 94
fgCode Fuchsia = 95
fgCode Aqua    = 96
fgCode White   = 97

-- | Convert a background color into an ANSI code.
bgCode
  :: Color -- ^ color
  -> Int   -- ^ ANSI code
bgCode Black   = 40
bgCode Maroon  = 41
bgCode Green   = 42
bgCode Olive   = 43
bgCode Navy    = 44
bgCode Purple  = 45
bgCode Teal    = 46
bgCode Silver  = 47
bgCode Default = 49
bgCode Gray    = 100
bgCode Red     = 101
bgCode Lime    = 102
bgCode Yellow  = 103
bgCode Blue    = 104
bgCode Fuchsia = 105
bgCode Aqua    = 106
bgCode White   = 107

-- | Convert a coloring scheme into an ANSI escape sequence.
translate
  :: Paint  -- ^ coloring scheme
  -> T.Text -- ^ escape sequence
translate (Paint fg bg flags) = T.concat [T.pack "\x1b[", codes, T.pack "m"]
  where
    codes  = T.intercalate (T.singleton ';') $ map (T.pack . show) codes'
    codes' = map flagCode flags ++ [fgCode fg] ++ [bgCode bg]

-- | Apply a color scheme to a text instance.
paint
  :: Paint  -- ^ coloring scheme
  -> T.Text -- ^ plain text
  -> T.Text -- ^ colorized text
paint pnt text = T.concat [translate pnt, text, T.pack "\x1b[0m"]
