# Text.Paint
[![Build Status](https://travis-ci.org/lovasko/paint.svg?branch=master)](https://travis-ci.org/lovasko/paint)
The `paint` module implements the essential subset of the ANSI terminal codes
that provide various text styling features, such as underlining, blinking or
different foreground and background coloring.

The motivation behind this module is to wrap the historic ANSI color codes
behind a simple and readable API that would allow clean and effect-bounded
styling of output based on the `text` module.

## Build & install
There are two standard ways of obtaining the module:
 * by cloning the git repository: `git clone https://github.com/lovasko/paint`
 * by using the central Hackage server: `cabal install paint`

### Dependencies
The library depends on two packages:
 * `base`
 * `text`

## API
### Types
The `Color` type is a simple enumeration of all 16 supported colors, plus the
setting to use the `Default` color set by the terminal. These colors can be
used for both the foreground and background layers. The full listing of
available color hues is as follows: `Black`, `Red`, `Green`, `Yellow`, `Blue`,
`Magenta`, `Cyan`, `LightGray`, `DarkGray`, `LightRed`, `LightGreen`,
`LightYellow`, `LightBlue`, `LightMagenta`, `LightCyan`, `White`, `Default`.

The `Flag` type enumerates features that are applicable to only one of the
layers, or to both at the same time. Currently there are three supported flags:
`Bold` for bold text (beware that some implementations treat this as a slightly
lighter version of selected colors), `Underline` for underlining the text and
`Blink` to achieve periodic (dis)appearance of the text (both layers) with a
frequency of less than 150 beats per minute.

The overarching type that defines the resulting styling combines the previous
two types is `Paint`, which is defined as `data Paint = Paint Color Color
[Flag]`.

### Functions
The only function exported by the `Text.Paint` module is `paint` with the
following prototype: `paint :: Paint -> Text -> Text`. This function can be
used to apply selected styling to a particular `Text`. All changes are reset to
the default state at the end of such text object and therefore the styling
effect is limited to the said `Text` instance.

## Example
The following code will act as a UNIX filter, while adding yellow and
red stylization to lines that start with `WARNING` and `ERROR` strings
respectively:
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.Paint
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Apply the paint to a line depending on its prefix.
colorize :: T.Text -- ^ line
         -> T.Text -- ^ colored line
colorize text
  | T.isPrefixOf "ERROR"   text = paint red    text
  | T.isPrefixOf "WARNING" text = paint yellow text
  | otherwise                   = text
  where
    red    = Paint White  Red     [Underline]
    yellow = Paint Yellow Default []

main :: IO ()
main = fmap (T.unlines . map colorize . T.lines) T.getContents >>= T.putStr
```

## License
The `paint` package is licensed under the terms of the [2-clause BSD
license](LICENSE). In case you need any other license, feel free to
contact the author.

## Author
Daniel Lovasko <daniel.lovasko@gmail.com>
