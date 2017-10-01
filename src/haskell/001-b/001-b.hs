import           Text.Printf

main :: IO ()
main =
  getLine >>=
  putStrLn . form . show . floor . solve . (\(d_m) -> d_m / 1000) . (read :: String -> Double)

form :: String -> String
form str
  | length str == 1 = "0" ++ str
  | otherwise       = str

solve :: Double -> Double
solve d_km
  | d_km < 0.1                =  0
  | d_km >= 0.1 && d_km <= 5  =  d_km * 10
  | d_km >= 6 && d_km <= 30   =  d_km + 50
  | d_km >= 35 && d_km <= 70  =  80 + (d_km - 30) / 5
  | d_km > 70                 =  89
