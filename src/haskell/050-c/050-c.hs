import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Text.Printf as Printf
import qualified Control.Monad as Monad
import qualified Data.Array as Array

bit :: Int -> [Int]
bit e = reverse $ bit' e
    where
    bit' 0 = []
    bit' x = (x `mod` 2) : bit' (x `div` 2)

modExp :: Int -> Int -> Int -> Int
modExp x e m = List.foldl' func 1 $ bit e
    where
    func n 0 = n^2 `mod` m
    func n 1 = n^2 `mod` m * x `mod` m

modNum :: Int
modNum = 1000000007

solve :: Int -> [Int] -> Int
solve n xs = if n `mod` 2 == 0 then
               solve' n 0 1 xs
             else
               solve' n 0 2 (tail xs)

solve' :: Int -> Int -> Int -> [Int] -> Int
solve' n _ _ [] = modExp 2 (n `div` 2) modNum
solve' n i c (x:xs) = if c == x then
                        if i `mod` 2 == 0 then
                          solve' n (i+1) c xs
                        else
                          solve' n (i+1) (c+2) xs
                      else
                        0

main :: IO ()
main = getContents >>=
  print .
  (\ (n,as) -> solve n as) .
  (\ [n,as] -> ((read :: String -> Int) n, List.sort . map (read :: String -> Int) . words $ as)) .
  lines
