import Data.List (permutations, group, sort)

canBePalindrome :: String -> Bool
canBePalindrome s
  | (odd . length) s = onlyOneGroupIsOdd
  | otherwise = all even lengths
    where groups = group . sort $ s
          onlyOneGroupIsOdd = (length oddLengths) == 1
          oddLengths = filter odd lengths
          lengths = map length groups

check :: String -> String
check s
  | canBePalindrome s = "YES"
  | otherwise = "NO"

main = interact check
