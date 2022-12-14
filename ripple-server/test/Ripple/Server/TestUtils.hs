{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Ripple.Server.TestUtils (rippleServerSpec) where

import Network.HTTP.Client as HTTP
import Ripple.API
import Ripple.DB
import Ripple.Server
import Servant.Client
import Test.Syd
import Test.Syd.Persistent.Sqlite
import Test.Syd.Servant as Servant
import Test.Syd.Wai

rippleServerSpec :: TestDef '[HTTP.Manager] ClientEnv -> Spec
rippleServerSpec =
  managerSpec . setupAroundWith' (\man () -> rippleClientEnvSetupFunc man)

rippleClientEnvSetupFunc :: HTTP.Manager -> SetupFunc ClientEnv
rippleClientEnvSetupFunc man = do
  env <- envSetupFunc
  let server = makeRippleServer env
  Servant.clientEnvSetupFunc completeAPI man server

envSetupFunc :: SetupFunc Env
envSetupFunc = do
  envConnectionPool <- connectionPoolSetupFunc automaticMigrations
  let envLogFunc _ _ _ _ = pure ()
  pure Env {..}
