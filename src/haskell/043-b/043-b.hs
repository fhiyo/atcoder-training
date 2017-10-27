main :: IO ()
main = getLine >>=
  putStrLn . (foldl (\res c -> if c == 'B' then if res == "" then res else reverse . tail . reverse $ res else res ++ [c]) "")
