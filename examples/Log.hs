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

import Data.List (isSubsequenceOf)
import qualified Text.Paint as P


-- | Apply the paint to a line depending on its prefix.
processEntry
  :: String -- ^ line
  -> String -- ^ colored line
processEntry text
  | isSubsequenceOf "ERROR" text = P.paint red    text
  | isSubsequenceOf "WARN"  text = P.paint yellow text
  | otherwise                    = text
  where
    red    = P.Paint P.White  P.Maroon  [P.Underline]
    yellow = P.Paint P.Yellow P.Default []

-- | Highlight errors and warnings in logs.
main
  :: IO () -- ^ action
main = fmap (unlines . map processEntry . lines) getContents >>= putStr
