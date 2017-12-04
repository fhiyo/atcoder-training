import Data.Array (listArray, (!))

lucas n = lucas' n where
  memo = listArray (0,n) $ fmap lucas' [0..n]
  lucas' 0 = 2
  lucas' 1 = 1
  lucas' n = memo ! (n-1) + memo ! (n-2)

main :: IO ()
main = getLine >>=
  print .
  lucas .
  (read :: String -> Int)
