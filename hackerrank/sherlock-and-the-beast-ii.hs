import Data.List (maximumBy)

greatestDecentNumber :: Integer -> String
greatestDecentNumber n
  | null combinations = "-1"
  | otherwise = displayCombination (maximumBy compareTuples combinations)
  where combinations = getCombinations n

displayCombination :: (Integer, Integer) -> String
displayCombination (threes, fives) = (takeN fives (repeat '5')) ++ (takeN threes (repeat '3'))

takeN :: Integer -> [a] -> [a]
takeN n = take (fromInteger n)

getCombinations :: Integer -> [(Integer, Integer)]
getCombinations n = [(x, y) | x <- [0..n], y <- [0..n], x + y == n, x `rem` 5 == 0, y `rem` 3 == 0]

compareTuples :: (Integer, Integer) -> (Integer, Integer) -> Ordering
compareTuples (threes, fives) (threes', fives')
  | fives > fives' = GT
  | (fives == fives') && (threes > threes') = GT
  | (threes == threes') && (fives == fives') = EQ
  | otherwise = LT

sherlock :: String -> String
sherlock s = let slines = lines s
                 t = read (head slines) :: Int
                 toProcess = take t $ tail slines
                 processed = map (greatestDecentNumber . read) toProcess
             in unlines processed

main = interact sherlock
