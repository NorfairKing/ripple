{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ripple.Server where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Coordinates
import Data.List
import Data.Time
import Data.UUID.Typed as UUID
import Database.Persist.Sqlite
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp (run)
import Ripple.API
import Ripple.DB
import Ripple.Frontend
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
        liftIO $ Warp.run 9701 application

makeRippleApplication :: Env -> Wai.Application
makeRippleApplication env = serve completeAPI (makeRippleServer env)

{-# ANN makeRippleServer ("NOCOVER" :: String) #-}
makeRippleServer :: Env -> Server CompleteAPI
makeRippleServer env =
  hoistServer completeAPI (runH env) rippleServer

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

rippleServer :: ServerT CompleteAPI H
rippleServer =
  serveHome
    :<|> serveUploadRipple
    :<|> serveListRipples
    :<|> serveGetRipple
    :<|> serveReRipple

serveHome :: H ByteString
serveHome = liftIO $ SB.readFile frontendHome

serveUploadRipple :: UploadRippleRequest -> H RippleUuid
serveUploadRipple UploadRippleRequest {..} = do
  rippleUuid <- nextRandomUUID
  let rippleType = uploadRippleRequestFileType
  let rippleContents = unRippleContent uploadRippleRequestFileContents
  let Coordinates {..} = uploadRippleRequestCoordinates
      rippleLatitude = coordinatesLat
      rippleLongitude = coordinatesLon
  let rippleOriginal = Nothing
  let rippleParent = Nothing
  rippleCreated <- liftIO getCurrentTime
  runDB $ insert_ Ripple {..}
  pure rippleUuid

serveListRipples :: Maybe Latitude -> Maybe Longitude -> H [RippleSummary]
serveListRipples mLat mLon =
  case Coordinates <$> mLat <*> mLon of
    Nothing -> throwError err400
    Just coordinates -> do
      now <- liftIO getCurrentTime
      let oldestRelevant = addUTCTime (-nominalDay) now
      ripples <- runDB $ selectList [RippleCreated >=. oldestRelevant] []
      pure $
        map
          ( \ripple ->
              let rippleSummaryId = rippleUuid ripple
                  rippleSummaryCoordinates = rippleCoordinates ripple
               in RippleSummary {..}
          )
          . nubBy
            (\r1 r2 -> ((==) <$> rippleOriginal r1 <*> rippleOriginal r2) == Just True)
          . filter (\r -> rippleCoordinates r `distanceTo` coordinates <= 1_000_000) -- 1000 km
          $ map entityVal ripples

serveGetRipple :: RippleUuid -> H RippleContent
serveGetRipple uuid = do
  mRipple <- runDB $ getBy $ UniqueRippleUuid uuid
  case mRipple of
    Nothing -> throwError err404
    Just (Entity _ ripple) -> pure $ RippleContent $ rippleContents ripple

serveReRipple :: ReRippleRequest -> H RippleUuid
serveReRipple ReRippleRequest {..} = do
  mRipple <- runDB $ getBy $ UniqueRippleUuid reRippleRequestId
  case mRipple of
    Nothing -> throwError err404
    Just (Entity _ ripple) -> do
      uuid <- nextRandomUUID
      now <- liftIO getCurrentTime
      let newRipple =
            ripple
              { rippleUuid = uuid,
                rippleCreated = now,
                rippleParent = Just $ rippleUuid ripple
              }
      runDB $ insert_ newRipple
      pure uuid
