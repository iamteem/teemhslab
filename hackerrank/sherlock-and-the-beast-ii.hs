greatestDecentNumber :: Integer -> String
greatestDecentNumber n = case c of
    Just t -> displayCombination t
    Nothing -> "-1"
  where c = getCombination n

displayCombination :: (Integer, Integer) -> String
displayCombination (threes, fives) = (takeN fives (repeat '5')) ++ (takeN threes (repeat '3'))

takeN :: Integer -> [a] -> [a]
takeN n = take (fromInteger n)

getCombination :: Integer -> Maybe (Integer, Integer)
getCombination n
  | null xs = Nothing
  | otherwise = Just (head xs)
  where xs = [(x, y) | x <- [0..n], y <- [n, (n - 1)..0], x + y == n, x `rem` 5 == 0, y `rem` 3 == 0]

sherlock :: String -> String
sherlock s = let slines = lines s
                 t = read (head slines) :: Int
                 toProcess = take t $ tail slines
                 processed = map (greatestDecentNumber . read) toProcess
             in unlines processed

main = interact sherlock
