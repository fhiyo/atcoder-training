import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Text.Printf as Printf
import qualified Control.Monad as Monad

getIntList :: IO [Int]
getIntList = map (read :: String -> Int) . words <$> getLine

main :: IO ()
main = do
  [n,m] <- getIntList
  acq <- map (\ [a,b] -> (a,b)) <$> Monad.replicateM m getIntList
  -- 全てのメンバの組み合わせを"List.subsequences [1..n]"で列挙し，
  -- あるメンバの組み合わせの全ての人間関係を"[(a,b) | a<-x, b<-x, a<b]"で列挙
  -- "filter (\ x -> all (`elem` acq) [人間関係])"で，
  -- あるメンバの組み合わせの人間関係が全て知り合いであるもののみを抽出している
  let res = maximum . map length $ filter (\ x -> all (`elem` acq) [(a,b) | a<-x, b<-x, a<b]) $ List.subsequences [1..n]
  print res
