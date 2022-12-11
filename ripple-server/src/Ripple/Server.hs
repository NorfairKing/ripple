{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ripple.Server (rippleServerMain) where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Coordinates
import Database.Persist.Sqlite
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp (run)
import Ripple.API
import Servant.API
import Servant.Multipart
import Servant.Server

rippleServerMain :: IO ()
rippleServerMain = do
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= LevelDebug) $
      withSqlitePool "ripple-server.sqlite3" 1 $ \pool -> do
        let envConnectionPool = pool
        envLogFunc <- askLoggerIO
        let env = Env {..}
        let application = makeRippleApplication env
        liftIO $ Warp.run 8000 application

{-# ANN makeRippleApplication ("NOCOVER" :: String) #-}
makeRippleApplication :: Env -> Wai.Application
makeRippleApplication env = serve rippleAPI $ hoistServer rippleAPI (runH env) rippleServer

runH :: Env -> H a -> Handler a
runH env func = runReaderT (unH func) env

newtype H a = H {unH :: ReaderT Env Handler a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

instance MonadLogger H where
  monadLoggerLog loc src lvl msg = do
    logFunc <- askLoggerIO
    liftIO $ logFunc loc src lvl (toLogStr msg)

instance MonadLoggerIO H where
  askLoggerIO = asks envLogFunc

data Env = Env
  { envConnectionPool :: !ConnectionPool,
    envLogFunc :: !(Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  }

rippleServer :: ServerT RippleAPI H
rippleServer =
  serveUploadRipple
    :<|> serveListRipples
    :<|> serveReRipple

serveUploadRipple :: Coordinates -> MultipartData Tmp -> H NoContent
serveUploadRipple _ _ = pure NoContent

serveListRipples :: Coordinates -> H [RippleSummary]
serveListRipples _ = pure []

serveReRipple :: Coordinates -> H NoContent
serveReRipple _ = pure NoContent
