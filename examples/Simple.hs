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

-- | All colors.
colors
  :: [P.Color] -- ^ all colors
colors =
  [ P.Black, P.Maroon,  P.Green, P.Olive
  , P.Navy,  P.Purple,  P.Teal,  P.Silver
  , P.Gray,  P.Red,     P.Lime,  P.Yellow
  , P.Blue,  P.Fuchsia, P.Aqua,  P.White]

-- | Compute the maximum length of all color names.
width
  :: Int -- ^ column width
width = maximum $ map (length . show) colors

-- | Justify a string to the left, filling the void with spaces.
justifyLeft
  :: Int    -- ^ intended width
  -> String -- ^ old string
  -> String -- ^ new string
justifyLeft width str
  | len > width = str
  | otherwise   = str ++ (replicate (width - len) ' ')
  where len = length str

-- | Print a single color entry containing the name.
printEntry
  :: Int     -- ^ column width
  -> P.Color -- ^ color
  -> IO ()   -- ^ action
printEntry cw clr = do
  putStr   $ justifyLeft (cw + 1) (show clr)
  putStrLn $ P.paint (P.Paint clr P.Default []) (show clr)

-- | Simple example detailing the available colors.
main
  :: IO () -- ^ action
main = mapM_ (printEntry width) colors
