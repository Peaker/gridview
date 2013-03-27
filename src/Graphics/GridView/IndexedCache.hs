module Graphics.GridView.IndexedCache where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Mutable as MV

data IndexedCache a = IndexedCache
  { _cacheSize :: Int
  , _cacheNew :: Int -> IO a
  , _cacheArray :: MV.IOVector (Maybe a)
  }

newDefault :: PrimMonad m => Int -> a -> m (MV.MVector (PrimState m) a)
newDefault l x = do
  v <- MV.new l
  forM_ [0..l-1] $ \i -> MV.write v i x
  return v

new :: Int -> (Int -> IO a) -> IO (IndexedCache a)
new count func = IndexedCache count func <$> newDefault count Nothing

debug :: String -> IO ()
debug _msg = return ()

delete :: IndexedCache a -> Int -> IO ()
delete (IndexedCache _ _ array) i = do
  maybe (return ()) (const . debug $ "Cache Deleting: " ++ show i) =<<
    MV.read array i
  MV.write array i Nothing

get :: IndexedCache a -> Int -> IO a
get (IndexedCache _ func array) index = do
  mItem <- MV.read array index
  case mItem of
    Nothing -> do
      item <- func index
      debug $ "Loading: " ++ show index
      MV.write array index $ Just item
      return item
    Just item -> return item
