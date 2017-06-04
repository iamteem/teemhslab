module Main where

import Data.Text (strip, pack, unpack)
import Data.Foldable (foldl')

process :: String -> String
process input =
  let vals       = lines input
      sidesCount = read $ head vals
      sides      = take sidesCount $ tail vals
      points     = map toPoint sides
      toSum      = tail points ++ take 1 points
  in show $ snd $ foldl' accDistance ((head points), 0) toSum

accDistance :: ((Integer, Integer), Float) -> (Integer, Integer) -> ((Integer, Integer), Float)
accDistance (lastPoint, perimeter) point = (point, perimeter + distanceBetween point lastPoint)

distanceBetween :: (Integer, Integer) -> (Integer, Integer) -> Float
distanceBetween (x1, y1) (x2, y2) = sqrt $ (fromInteger x2 - fromInteger x1) ** 2 + (fromInteger y2 - fromInteger y1) ** 2

toPoint :: String -> (Integer, Integer)
toPoint s  = let (xt, yt) = break (==' ') s
                 x = read xt :: Integer
                 yt' = unpack $ strip $ pack yt
                 y = read yt' :: Integer
             in (x, y)

main = interact process >> return ()

