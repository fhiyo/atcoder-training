main :: IO ()
main = getContents >>=
  print .
  (\(res1,res2) -> if res1 < res2 then res1 else res2) .
  (\(n,l) -> (sum . map (\v -> (v-n)*(v-n)) $ l,
              sum . map (\v -> (v-(n+1))*(v-(n+1))) $ l)) .
  (\(n,l) -> (sum l `quot` n, l)) .
  (\list -> (head list, tail list)) .
  map (read :: String -> Int) .
  words
