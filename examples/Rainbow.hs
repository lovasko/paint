import Text.Paint as P
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Maybe (maybe)
import Data.List (find)

-- | Approximation of rainbow rings.
rainbow
  :: [(Int, P.Color)] -- ^ ring index & color
rainbow = zip [0 ..] [P.Purple, P.Blue, P.Lime, P.Yellow, P.Red]

-- | Image width.
width
  :: Int -- ^ width
width = 64 

-- | Image height.
height
  :: Int -- ^ height
height = 20

-- | Rainbow arc radius.
radius
  :: Int -- ^ radius
radius = 8

-- | Building block of the image.
block
  :: T.Text
block = T.singleton '*'

-- | Determine a color of an image cell.
cellColor
  :: Int     -- ^ row
  -> Int     -- ^ column
  -> P.Color -- ^ color
cellColor row col = maybe P.White snd (find match rainbow)
  where
    match (idx, _) = idx + radius == dist
    dist           = floor $ sqrt $ fromIntegral $ line
    line           =  ((row - height) ^ 2) + ((col - (div width 2)) ^ 2)

createRow
  :: Int    -- ^ row index
  -> T.Text -- ^ row text
createRow row = T.concat $ map cell [0 .. width]
  where
    cell col = P.paint (P.Paint (cellColor row col) P.White []) block

-- | Draw a jolly rainbow!
main
  :: IO () -- ^ action
main = do
  mapM_ T.putStrLn (map createRow [0 .. height])
  T.putStrLn $ P.paint (P.Paint P.Green P.White []) $ T.replicate (succ width) block
