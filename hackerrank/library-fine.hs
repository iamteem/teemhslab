import Data.Time.Calendar

parseDate :: String -> Day
parseDate s = fromGregorian y m d
                 where d = read $ ts !! 0
                       m = read $ ts !! 1
                       y = read $ ts !! 2
                       ts = words s

computeFine :: Day -> Day -> Integer
computeFine actual expected
  | d <= 0 = 0
  | actualY /= expectedY = 10000
  | actualM /= expectedM = 500 * toInteger (actualM - expectedM)
  | otherwise = 15 * d
    where d = diffDays actual expected
          (actualY, actualM, _) = toGregorian actual
          (expectedY, expectedM, _) = toGregorian expected

solve :: String -> String
solve s = show $ computeFine actual expected
            where actual = parseDate $ dates !! 0
                  expected = parseDate $ dates !! 1
                  dates = lines s

main = interact solve
