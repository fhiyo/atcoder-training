import Data.List

main :: IO ()
main = getLine >>=
  putStrLn . (\l -> if l == ["5","5","7"] then "YES" else "NO") . sort . words
