import Data.List

main :: IO ()
main = getContents >>=
  mapM_ putStr . (\l -> init l ++ [last l ++ "\n"]) . sort . tail . lines
