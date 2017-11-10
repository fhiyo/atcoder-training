import qualified Control.Monad as Monad
import qualified Data.Vector.Unboxed as UV

main :: IO ()
main = do
  _ <- getLine
  ts <- UV.fromList . fmap (read :: String -> Int) . words <$> getLine
  m <- readLn :: IO Int
  Monad.replicateM_ m $ do
    [p,x] <- map (read :: String -> Int) . words <$> getLine
    print . UV.sum $ ts UV.// [(p-1,x)]
