import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe

-- `iterate succ n`でn以上の数値をばんばんlistで作って，使っちゃいけない数値を1つも使ってない最小の数値 (一番最初にlist内に出てきた数値) を出力すればよい！
-- 遅延評価だからできる芸当．

main :: IO ()
main = getContents >>=
  print . (\(n:_:dislike_digits) -> Maybe.maybe 0 id . List.find (null . List.intersect dislike_digits . map Char.digitToInt . show) $ iterate succ n) . map (read :: String -> Int) . words
