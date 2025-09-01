module App where

import Control.Monad.Reader
import Logger

newtype AppEnv = AppEnv { debugLogger :: Logger}

newtype AppM a = AppM { runAppM :: ReaderT AppEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

runAppMStart :: AppEnv -> AppM a -> IO a
runAppMStart env (AppM r) = runReaderT r env