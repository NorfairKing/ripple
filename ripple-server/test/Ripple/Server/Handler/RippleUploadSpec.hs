{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Ripple.Server.Handler.RippleUploadSpec (spec) where

import Network.HTTP.Client as HTTP
import Ripple.API
import Ripple.Client
import Ripple.Server
import Servant.API
import Servant.Client
import Test.Syd
import Test.Syd.Persistent.Sqlite
import Test.Syd.Servant as Servant
import Test.Syd.Validity
import Test.Syd.Wai

rippleServerSpec :: TestDef '[HTTP.Manager] ClientEnv -> Spec
rippleServerSpec =
  managerSpec . setupAroundWith' (\man () -> rippleClientEnvSetupFunc man)

rippleClientEnvSetupFunc :: HTTP.Manager -> SetupFunc ClientEnv
rippleClientEnvSetupFunc man = do
  env <- envSetupFunc
  let server = makeRippleServer env
  Servant.clientEnvSetupFunc rippleAPI man server

envSetupFunc :: SetupFunc Env
envSetupFunc = do
  envConnectionPool <- connectionPoolSetupFunc (pure ())
  let envLogFunc _ _ _ _ = pure ()
  pure Env {..}

spec :: Spec
spec = rippleServerSpec $ do
  it "can upload a ripple" $ \cenv ->
    forAllValid $ \rippleUpload -> do
      NoContent <- testClient cenv $ clientUploadRipple rippleUpload
      pure ()
