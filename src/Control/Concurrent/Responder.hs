{-# LANGUAGE ExistentialQuantification #-}
module Control.Concurrent.Responder
  ( Responder
  , new
  , handleRequests
  , sendRequest
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import qualified Control.Exception as E

data Request =
  forall a.
  Request
  { _reqAction :: IO a
  , _reqResponseMVar :: MVar (Either E.SomeException a)
  }

newtype Responder = Responder (MVar [Request])

handleRequest :: Request -> IO ()
handleRequest (Request action responseMVar) =
  putMVar responseMVar =<< E.try action

handleRequests :: Responder -> IO ()
handleRequests (Responder requestsMVar) = do
  requests <- modifyMVar requestsMVar $ \requests -> return ([], requests)
  mapM_ handleRequest requests

sendRequest :: Responder -> IO a -> IO a
sendRequest (Responder requestsMVar) action = do
  response <- newEmptyMVar
  modifyMVar_ requestsMVar $ return . (Request action response :) 
  either E.throwIO return =<< takeMVar response

new :: IO Responder
new = Responder <$> newMVar []
