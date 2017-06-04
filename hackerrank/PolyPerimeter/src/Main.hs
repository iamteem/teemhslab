module Main where

import Data.Text (strip, pack, unpack)
import Data.Foldable (foldl')

process :: String -> String
process input =
  let sidesCount = read $ head vals
      sides      = tail vals
      vals       = lines input
      points     = map toPoint $ take sidesCount sides
      toSum      = points ++ take 1 points
  in show $ snd $ foldl' foldFun ((head toSum), 0) toSum

foldFun :: ((Integer, Integer), Float) -> (Integer, Integer) -> ((Integer, Integer), Float)
foldFun (lastPoint, peri) point = (point, peri + dist)
  where dist = distanceBetween point lastPoint

distanceBetween :: (Integer, Integer) -> (Integer, Integer) -> Float
distanceBetween (x1, y1) (x2, y2) = sqrt $ (fromInteger x2 - fromInteger x1) ** 2 + (fromInteger y2 - fromInteger y1) ** 2

toPoint :: String -> (Integer, Integer)
toPoint s  = let (xt, yt) = break (==' ') s
                 x = read xt :: Integer
                 yt' = unpack $ strip $ pack yt
                 y = read yt' :: Integer
             in (x, y)

main = interact process >> return ()

