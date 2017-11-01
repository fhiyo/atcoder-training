import qualified Text.Printf as Printf

main :: IO ()
main = getLine >>=
  (\ res -> Printf.printf "%.3f\n" res) .
  (\ (v1_x,v1_y,v2_x,v2_y) -> abs ((v1_x * v2_y - v1_y * v2_x) / 2)) .
  (\ [a,b,c,d,e,f] -> (c-a,d-b,e-a,f-b)) .
  map (read :: String -> Double) .
  words
