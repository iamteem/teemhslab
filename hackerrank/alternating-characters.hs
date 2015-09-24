import Data.List (group)

processLine :: String -> Integer
processLine s = sum $ map getDeletions groups
  where groups = group s
        getDeletions w = (toInteger . length $ w) - 1

processInput :: String -> String
run s = unlines $ map (show . processLine) to_process
  where t = read (head ls) :: Int
        ls = lines s
        to_process = take t $ tail ls

main = interact processInput
