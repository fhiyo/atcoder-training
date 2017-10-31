main :: IO ()
main = getLine >>=
  putStrLn . filter (\ x -> not $ elem x "aiueo")
