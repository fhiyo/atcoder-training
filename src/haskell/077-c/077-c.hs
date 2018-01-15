-- ref: https://codereview.stackexchange.com/questions/180195/programming-contest-snuke-festival#answer-184938

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Text (Text)
import qualified Data.List as L

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed.Mutable as VMut

import Control.Monad  ( replicateM )

import System.IO.Unsafe
import Data.Bits (shiftR)

-- adapted from "vector-algorithms" package:

{-# INLINE binarySearchPBounds #-}
binarySearchPBounds p vec = loop
 where
 loop !l !u
   | u <= l    = return l
   | otherwise = VMut.unsafeRead vec k >>= \e -> if p e then loop l k else loop (k+1) u
  where k = (u + l) `shiftR` 1

{-# INLINE binarySearchL #-}
binarySearchL vec e = binarySearchPBounds (>= e) vec 0 (VMut.length vec)

{-# INLINE binarySearchR #-}
binarySearchR vec e = binarySearchPBounds (> e) vec 0 (VMut.length vec)



forceEither :: Either a b -> b
forceEither (Right r) = r 

parseLine :: Text -> [Int]
parseLine txt =
  map (fst . forceEither . T.decimal) $ T.splitOn (T.pack " ") txt

bisectLeft :: Int -> Vector Int -> Int
bisectLeft n xs = unsafePerformIO $ do
  xs' <- V.unsafeThaw xs
  binarySearchL xs' n

bisectRight :: Int -> Vector Int -> Int
bisectRight n xs = unsafePerformIO $ do 
  xs' <- V.unsafeThaw xs
  binarySearchR xs' n

getInput :: IO [Text]  
getInput = do
  res@[n, as, bs, cs] <- replicateM 4 T.getLine
  return res

sortedVec :: t -> Text -> Vector Int
sortedVec n txt = 
  V.fromList $ L.sort $ parseLine txt


parseProblemInput
  :: [Text] -> (Int, Vector Int, [Int], Vector Int)
parseProblemInput [nStr, asStr, bsStr, csStr] = 
  let [n] = parseLine nStr
      as =  sortedVec n asStr
      bs =  parseLine  bsStr
      cs = sortedVec n csStr
  in (n, as, bs, cs)

solve :: (Int, Vector Int, [Int], Vector Int) -> Int
solve (n, as, bs, cs) = 
  sum $ map (\ b -> bisectLeft b as * (n - bisectRight b cs)) bs


main = do
  ls <- getInput 
  print $ solve $ parseProblemInput ls
