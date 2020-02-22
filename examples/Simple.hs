{- |
Module      : Main
Description : Simple example that lists available colors.
Copyright   : (c) 2017-2020 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Text.Paint as P
import qualified Data.Text as T
import qualified Data.Text.IO as T


-- | All colors.
colors
  :: [P.Color] -- ^ all colors
colors =
  [ P.Black,        P.Red,          P.Green,       P.Yellow
  , P.Blue,         P.Magenta,      P.Cyan,        P.LightGray
  , P.DarkGray,     P.LightRed,     P.LightGreen,  P.LightYellow
  , P.LightBlue,    P.LightMagenta, P.LightCyan,   P.White]

-- | Compute the maximum length of all color names.
width
  :: Int -- ^ column width
width = maximum $ map (length . show) colors

-- | Print a single color entry containing the name.
printEntry
  :: Int     -- ^ column width
  -> P.Color -- ^ color
  -> IO ()   -- ^ action
printEntry cw clr = do
  T.putStr   $ T.justifyLeft (cw + 1) ' ' $ T.pack $ show clr
  T.putStrLn $ P.paint (P.Paint clr P.Default []) (T.pack $ show clr)

-- | Simple example detailing the available colors.
main
  :: IO () -- ^ action
main = mapM_ (printEntry width) colors
