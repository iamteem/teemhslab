fac :: String -> String
fac s = show $ product [1..n]
  where n = read s :: Integer

main = interact fac
