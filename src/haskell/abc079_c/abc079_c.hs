import Data.Char as Char
import Control.Monad as Monad

solve :: [Int] -> [(Int, String)]
solve (n:ns) = map (foldl
                     (\ (acc,s) (op,digit) -> (toFunc op acc digit, s ++ op : show digit))
                      (n, show n)) $
                        map (`zip` ns) $ Monad.replicateM 3 ['+', '-']
               where toFunc c
                       | c == '+'     = (+)
                       | otherwise    = (-)

main :: IO ()
main = getLine >>=
  putStrLn .
  (\ ans -> (snd $ head ans) ++ "=7") .
  filter ((==7) . fst) .
  solve .
  map digitToInt
