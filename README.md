# Text.Paint
The `paint` module implements the essential subset of the ANSI terminal codes
that provide various text styling features, such as underlining, blinking or
different foreground and background coloring.

TODO reason

## Build & install
There are two standard ways of obtaining the module:
 * by cloning the git repository: `git clone https://github.com/lovasko/paint`
 * by using the central Hackage server: `cabal install paint`

### Dependencies
The library depends on two packages:
 * `base`
 * `text`

## API
TODO

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
