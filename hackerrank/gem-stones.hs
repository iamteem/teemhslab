countGemElements :: [String] -> Integer
countGemElements ges = toInteger $ length foundElements
  where foundElements = filter foundInAllGemElements elements
        foundInAllGemElements e = all (\ge -> e `elem` ge) ges
        elements = ['a'..'z']

gemElementsCount :: String -> String
gemElementsCount s = show $ countGemElements rocks
  where n = read $ head rockLines :: Int
        rocks = take n $ tail rockLines
        rockLines = lines s

main = interact gemElementsCount
