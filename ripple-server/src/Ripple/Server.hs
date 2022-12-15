{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ripple.Server where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Coordinates
import Data.Maybe
import Data.Time
import Data.UUID.Typed as UUID
import Database.Persist.Sqlite
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp (run)
import Ripple.API
import Ripple.DB
import Servant

rippleServerMain :: IO ()
rippleServerMain = do
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= LevelDebug) $
      withSqlitePool "ripple-server.sqlite3" 1 $ \pool -> do
        let envConnectionPool = pool
        runSqlPool (runMigration automaticMigrations) pool
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

runDB :: SqlPersistM a -> H a
runDB query = do
  pool <- asks envConnectionPool
  liftIO $ runSqlPersistMPool (retryOnBusy query) pool

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
serveUploadRipple UploadRippleRequest {..} = do
  rippleUuid <- nextRandomUUID
  let rippleType = uploadRippleRequestFileType
  let rippleContents = unRippleContent uploadRippleRequestFileContents
  let Coordinates {..} = uploadRippleRequestCoordinates
      rippleLatitude = coordinatesLat
      rippleLongitude = coordinatesLon
  let rippleOriginal = Nothing
  rippleCreated <- liftIO getCurrentTime
  runDB $ insert_ Ripple {..}
  pure rippleUuid

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
serveGetRipple uuid = do
  mRipple <- runDB $ getBy $ UniqueRippleUuid uuid
  case mRipple of
    Nothing -> throwError err404
    Just (Entity _ ripple) -> pure $ RippleContent $ rippleContents ripple

serveReRipple :: ReRippleRequest -> H RippleUuid
serveReRipple _ = pure exampleUuid
