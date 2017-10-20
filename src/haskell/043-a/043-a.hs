main :: IO ()
main = getLine >>= print . (\n -> sum [1..n]) . (read :: String -> Int)
