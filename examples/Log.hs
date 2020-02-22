{- |
Module      : Main
Description : Example that highlights errors and warnings in logs.
Copyright   : (c) 2017-2020 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

import Text.Paint
import qualified Data.Text as T
import qualified Data.Text.IO as T


-- | Apply the paint to a line depending on its prefix.
processEntry
  :: T.Text -- ^ line
  -> T.Text -- ^ colored line
processEntry text
  | T.isPrefixOf "ERROR" text = paint red    text
  | T.isPrefixOf "WARN"  text = paint yellow text
  | otherwise                 = text
  where
    red    = Paint White  Red     [Underline]
    yellow = Paint Yellow Default []

-- | Highlight errors and warnings in logs.
main
  :: IO () -- ^ action
main = fmap (T.unlines . map processEntry . T.lines) T.getContents >>= T.putStr
