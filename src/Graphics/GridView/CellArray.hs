module Graphics.GridView.CellArray (CellArray, new, delete, get, startComputing) where

import Control.Applicative
import Control.Concurrent.Cell (Cell)
import Data.Vector ((!))
import qualified Control.Concurrent.Cell as Cell
import qualified Data.Vector as V

newtype CellArray a = CellArray
  { _cacheArray :: V.Vector (Cell a)
  }

new :: Int -> (Int -> IO a) -> IO (CellArray a)
new count func =
  CellArray . V.fromListN count <$>
  mapM (Cell.new . func) [0..count-1]

delete :: CellArray a -> Int -> IO ()
delete (CellArray array) i = Cell.delete $ array ! i

startComputing :: CellArray a -> Int -> IO ()
startComputing (CellArray array) i = Cell.startComputing $ array ! i

get :: CellArray a -> Int -> IO (Maybe a)
get (CellArray array) i = Cell.peek $ array ! i
