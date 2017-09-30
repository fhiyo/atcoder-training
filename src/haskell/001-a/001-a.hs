main :: IO ()
main =
  getContents >>=
  putStrLn . show . (\[h1, h2] -> h1 - h2) . map (read :: String -> Int) . words
