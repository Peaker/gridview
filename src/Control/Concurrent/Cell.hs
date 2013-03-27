module Control.Concurrent.Cell
  ( Cell
  , new
  , startComputing
  , delete
  , peek
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, killThread, forkIO)
import Control.Concurrent.MVar

data CellState a = Uncomputed | Computed a | Computing ThreadId

data Cell a = Cell
  { _cellCompute :: IO a
  , _cellMVar :: MVar (CellState a)
  }

new :: IO a -> IO (Cell a)
new action = Cell action <$> newMVar Uncomputed

startComputing :: Cell a -> IO ()
startComputing (Cell compute mvar) =
  modifyMVar_ mvar start
  where
    start x@(Computed _) = return x
    start x@(Computing _) = return x
    start    Uncomputed =
      fmap Computing . forkIO $ do
        result <- Computed <$> compute
        modifyMVar_ mvar . const $ return result

delete :: Cell a -> IO ()
delete (Cell _ mvar) = do
  modifyMVar_ mvar del
  where
    del Uncomputed = return Uncomputed
    del (Computed _) = return Uncomputed
    del (Computing tid) = do
      killThread tid
      return Uncomputed

peek :: Cell a -> IO (Maybe a)
peek (Cell _ mvar) = f <$> readMVar mvar
  where
    f Uncomputed = Nothing
    f (Computing _) = Nothing
    f (Computed x) = Just x
