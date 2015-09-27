import Data.List (find)

fThreesAndFives :: String -> Bool
fThreesAndFives s = all (\c -> c `elem` ['3', '5']) s

validThrees :: String -> Bool
validThrees s = (count '3' s) `mod` 5 == 0

validFives :: String -> Bool
validFives s = (count '5' s) `mod` 3 == 0

count :: Eq a => a -> [a] -> Int
count x = (length . filter (x==))

findDecentNumber :: [Integer] -> String
findDecentNumber xs = maybe "-1" id m
  where m = find (\s -> fThreesAndFives s && validThrees s && validFives s) (map show xs)

genRange :: Integer -> [Integer]
genRange n = [fives, (fives - 1)..threes]
  where repN = (take $ fromInteger n) . repeat
        threes = read (repN '3') :: Integer
        fives = read (repN '5') :: Integer

sherlock :: String -> String
sherlock s = let slines = lines s
                 t = read (head slines) :: Int
                 toProcess = take t $ tail slines
                 processed = map (findDecentNumber . genRange . read) toProcess
             in unlines processed

main = interact sherlock
