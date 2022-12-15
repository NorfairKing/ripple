{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ripple.Server where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Coordinates
import Data.Maybe
import Data.UUID.Typed as UUID
import Database.Persist.Sqlite
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp (run)
import Ripple.API
import Servant

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

makeRippleApplication :: Env -> Wai.Application
makeRippleApplication env = serve rippleAPI (makeRippleServer env)

{-# ANN makeRippleServer ("NOCOVER" :: String) #-}
makeRippleServer :: Env -> Server RippleAPI
makeRippleServer env =
  hoistServer rippleAPI (runH env) rippleServer

runH :: Env -> H a -> Handler a
runH env func = runReaderT (unH func) env

newtype H a = H {unH :: ReaderT Env Handler a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError ServerError)

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
    :<|> serveGetRipple
    :<|> serveReRipple

exampleUuid :: RippleUuid
exampleUuid = fromJust $ UUID.parseUUIDString "70316487-345c-449e-a390-b0406b9292a9"

exampleCoordinates :: Coordinates
exampleCoordinates =
  Coordinates
    { coordinatesLat = Latitude 0,
      coordinatesLon = Longitude 0
    }

serveUploadRipple :: UploadRippleRequest -> H RippleUuid
serveUploadRipple _ = pure exampleUuid

serveListRipples :: Maybe Latitude -> Maybe Longitude -> H [RippleSummary]
serveListRipples mLat mLon =
  case Coordinates <$> mLat <*> mLon of
    Nothing -> throwError err400
    Just coordinates ->
      pure
        [ RippleSummary
            { rippleSummaryId = exampleUuid,
              rippleSummaryCoordinates = coordinates
            }
        ]

serveGetRipple :: RippleUuid -> H RippleContent
serveGetRipple _ = pure $ RippleContent mempty

serveReRipple :: ReRippleRequest -> H RippleUuid
serveReRipple _ = pure exampleUuid
