import qualified Data.List as List
import qualified Data.IntMap.Strict as IntMap

bit :: Integer -> [Integer]
bit e = reverse $ bit' e
    where
    bit' 0 = []
    bit' x = (x `mod` 2) : bit' (x `div` 2)

modExp :: Integer -> Integer -> Integer -> Integer
modExp x e m = List.foldl' func 1 $ bit e
    where
    func n 0 = n^2 `mod` m
    func n 1 = n^2 `mod` m * x `mod` m

modNum :: Integer
modNum = 1000000007

factTable :: Integer -> [Integer]
factTable n = reverse $ factTable' n

factTable' :: Integer -> [Integer]
factTable' n
  | n < 0     = error "arg n must not be negative number!"
  | n == 0    = [1]
  | otherwise = (((head $ factTable' (n - 1)) * n) `mod` modNum) : factTable' (n - 1)

invFactTable :: [Integer] -> [Integer]
invFactTable fact_table = map (\fact -> modExp fact 1000000005 1000000007) fact_table

-- FIXME: 上手く動作しない．渡す引数をIntからIntegral aに変換してるから？
comb ::  Integer -> Integer -> IntMap.IntMap Integer -> IntMap.IntMap Integer -> Integer
comb n r fact_table inv_fact_table = ((fact_table IntMap.! n) * (inv_fact_table IntMap.! r) * (inv_fact_table IntMap.! (n - r))) `mod` modNum

main :: IO ()
-- main = do
--   line <- getLine
--   let [h,w,a,b] = map (read :: (Integral a) => String -> a) . words $ line
--   print [h,w,a,b]
  -- print [h,w,a,b]
main = getLine >>=
  print .
    (\n -> mod n modNum) .
    -- (\(h,w,a,b,fs,inv_fs) -> comb 5 2 fs inv_fs) .
    (\(h,w,a,b,m_fs,m_inv_fs) -> foldl (+) 0 $
      map (\i -> comb (h-a+i-1) (h-a-1) m_fs m_inv_fs *
                 comb (w+a-i-2) (a-1) m_fs m_inv_fs)
      [b..w-1]) .
    (\(h,w,a,b,fs,inv_fs) -> (h,w,a,b,IntMap.fromList $ zip [0..length fs - 1] fs,IntMap.fromList $ zip [0..length inv_fs - 1] inv_fs)) .
    (\(h,w,a,b,fs) -> (h,w,a,b,fs,invFactTable fs)) .
    (\(h,w,a,b) -> (h,w,a,b,factTable (h+w-2))) .
    (\[h,w,a,b] -> ((read :: String -> Integer) h, (read :: String -> Integer) w, (read :: String -> Int) a, (read :: String -> Int) b))  .
    words

-- bit :: (Integral a) => a -> [a]
-- bit e = reverse $ bit' e
--     where
--     bit' 0 = []
--     bit' x = (x `mod` 2) : bit' (x `div` 2)
--
-- modExp :: (Integral a) => a -> a -> a -> a
-- modExp x e m = List.foldl' func 1 $ bit e
--     where
--     func n 0 = n^2 `mod` m
--     func n 1 = n^2 `mod` m * x `mod` m
--
-- modNum :: (Integral a) => a
-- modNum = 1000000007
--
-- factTable :: (Integral a) => a -> [a]
-- factTable n = reverse $ factTable' n
--
-- factTable' :: (Integral a) => a -> [a]
-- factTable' n
--   | n < 0     = error "arg n must not be negative number!"
--   | n == 0    = [1]
--   | otherwise = (((head $ factTable' (n - 1)) * n) `mod` modNum) : factTable' (n - 1)
--
-- invFactTable :: (Integral a) => [a] -> [a]
-- invFactTable fact_table = map (\fact -> modExp fact 1000000005 1000000007) fact_table
--
-- -- FIXME: 上手く動作しない．渡す引数をIntからIntegral aに変換してるから？
-- comb :: (Integral a) => Int -> Int -> [a] -> [a] -> a
-- comb n r fact_table inv_fact_table = ((fact_table !! n) * (inv_fact_table !! r) * (inv_fact_table !! (n - r))) `mod` modNum
--
-- main :: IO ()
-- main = getLine >>=
--   print .
--     (\n -> mod n modNum) .
--     (\(h,w,a,b,fs,inv_fs) -> foldl (+) 0 $
--       map (\i -> comb ((h-a+i-1) :: Int) ((h-a-1) :: Int) fs inv_fs *
--                  comb ((w+a-i-2) :: Int) ((a-1) :: Int) fs inv_fs)
--        [b..w-1]) .
--     (\(h,w,a,b,fs) -> (h,w,a,b,fs,invFactTable fs)) .
--     (\[h,w,a,b] -> (h,w,a,b,factTable (h+w-2))) .
--     map (read :: (Read a, Integral a) => String -> a) .
--     words

    -- (\n -> mod n modNum) . (\(h,w,a,b,m_fs,m_inv_fs) -> foldl (+) 0 $ map (\i -> comb (h-a+i-1) (h-a-1) m_fs m_inv_fs * comb (w+a-i-2) (a-1) m_fs m_inv_fs) [b..w-1]) .  (\(h,w,a,b,fs,inv_fs) -> (h,w,a,b,IntMap.fromList $ zip [0..length fs - 1] fs,IntMap.fromList $ zip [0..length inv_fs - 1] inv_fs)) .  (\(h,w,a,b,fs) -> (h,w,a,b,fs,invFactTable fs)) .  (\[h,w,a,b] -> (h,w,a,b,factTable (h+w-2))) .  map (read :: String -> Int) .  words
