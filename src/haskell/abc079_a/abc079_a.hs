main :: IO ()
main = getLine >>=
  putStrLn .
  (\ ds -> if (ds !! 0 == ds !! 1) && (ds !! 1 == ds !! 2) ||
              (ds !! 1 == ds !! 2) && (ds !! 2 == ds !! 3) then "Yes"
           else "No")
