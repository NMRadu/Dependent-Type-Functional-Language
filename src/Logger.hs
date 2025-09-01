module Logger where

import Control.Concurrent
import Control.Concurrent.MVar

data LogCommand = Message String | Stop (MVar ())

data Logger = Logger
  { logChan   :: MVar LogCommand
  , logActive :: Bool
  }

initLogger :: Bool -> IO Logger
initLogger shouldLog = do
    m <- newEmptyMVar
    let logger = Logger m shouldLog
    forkIO (loggerThread logger)
    return logger

logMessage :: Logger -> String -> IO ()
logMessage (Logger m _) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m _) = do
    s <- newEmptyMVar
    putMVar m (Stop s)
    takeMVar s

loggerThread :: Logger -> IO ()
loggerThread (Logger m active) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          if active then mapM_ putStrLn (lines msg) else pure ()
          loop
        Stop s -> do
          if active then putStrLn "The program has finished" else pure ()
          putMVar s ()