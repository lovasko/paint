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
  | Red     -- ^ red
  | Green   -- ^ green
  | Yellow  -- ^ yellow
  | Blue    -- ^ blue
  | Magenta -- ^ magenta
  | Cyan    -- ^ cyan
  | White   -- ^ white
  | Default -- ^ default setting
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
fgCode Default = 39
fgCode color   = 30 + fromEnum color

-- | Convert a background color into an ANSI code.
bgCode :: Color -- ^ color
       -> Int   -- ^ ANSI code
bgCode Default = 49
bgCode color   = 40 + fromEnum color

-- | Convert a coloring scheme into an ANSI escape sequence.
translate :: Paint  -- ^ coloring scheme
          -> T.Text -- ^ escape sequence
translate (Paint fg bg flags) = T.concat [T.pack "\x1b[", codes, T.pack "m"]
  where
    codes  = T.intercalate ";" $ map (T.pack . show) codes
    codes' = map flagCode flags ++ [fgCode fg] ++ [bgCode bg]

-- | Apply a color scheme to a text instance.
paint :: Paint  -- ^ coloring scheme
      -> T.Text -- ^ plain text
      -> T.Text -- ^ colorized text
paint pnt text = T.concat [translate pnt, text, T.pack "\x1b[0m"]
