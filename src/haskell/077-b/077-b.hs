import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Text.Printf as Printf
import qualified Control.Monad as Monad

main :: IO ()
main = getLine >>=
  print .
  (\ x -> x * x) .
  floor .
  sqrt .
  (read :: String -> Double)
