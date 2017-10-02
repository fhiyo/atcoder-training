import Text.Printf

main :: IO ()
main = getLine >>=
  putStrLn . (\[deg, ws] -> convertDirection (fromIntegral deg) ws ++ " " ++ show ws) . (\[deg, dis] -> [deg, (windPower $ ((fromIntegral dis) / 60.0))]) <$> map (read :: String -> Int) . words

convertDirection :: Double -> Int -> String
convertDirection deg ws
  | ws == 0       =  "C"
  | deg < 112.5   =  "N"
  | deg < 337.5   =  "NNE"
  | deg < 562.5   =  "NE"
  | deg < 787.5   =  "ENE"
  | deg < 1012.5  =  "E"
  | deg < 1237.5  =  "ESE"
  | deg < 1462.5  =  "SE"
  | deg < 1687.5  =  "SSE"
  | deg < 1912.5  =  "S"
  | deg < 2137.5  =  "SSW"
  | deg < 2362.5  =  "SW"
  | deg < 2587.5  =  "WSW"
  | deg < 2812.5  =  "W"
  | deg < 3037.5  =  "WNW"
  | deg < 3262.5  =  "NW"
  | deg < 3487.5  =  "NNW"
  | otherwise     =  "N"


windPower :: Double -> Int
windPower wind_speed
  | wind_speed < 0.25   =  0
  | wind_speed < 1.55   =  1
  | wind_speed < 3.35   =  2
  | wind_speed < 5.45   =  3
  | wind_speed < 7.95   =  4
  | wind_speed < 10.75  =  5
  | wind_speed < 13.85  =  6
  | wind_speed < 17.15  =  7
  | wind_speed < 20.75  =  8
  | wind_speed < 24.45  =  9
  | wind_speed < 28.45  = 10
  | wind_speed < 32.65  = 11
  | otherwise           = 12

-- main = getLine >>=
--   putStrLn . (\l -> show (head l) ++ " " ++ show (head $ tail l)) <$> map (read :: String -> Int) . words

-- main :: IO ()
-- main = getLine >>=
  -- mapM_ print . (\[deg, dis] -> [deg, (windPower $ ((fromIntegral dis) / 60.0))]) <$> map (read :: String -> Int) . words
