{-# LANGUAGE RecordWildCards #-}

module Ripple.Server.Handler.ScenarioSpec (spec) where

import Ripple.API
import Ripple.Client
import Ripple.Server.TestUtils
import Servant.API
import Test.Syd
import Test.Syd.Servant as Servant
import Test.Syd.Validity

spec :: Spec
spec = rippleServerSpec $ do
  it "can upload a ripple, then find it, and re-ripple it" $ \cenv ->
    forAllValid $ \uploadRippleRequest -> do
      testClient cenv $ do
        NoContent <- clientUploadRipple uploadRippleRequest
        let coordinates = uploadRippleRequestCoordinates uploadRippleRequest
        ripples <- clientListRipples coordinates
        uuid <- case ripples of
          [RippleSummary {..}] -> pure rippleSummaryId
          _ -> liftIO $ expectationFailure "Expected exactly one ripple"
        let rerippleRequest =
              ReRippleRequest
                { reRippleRequestCoordinates = coordinates,
                  reRippleRequestId = uuid
                }
        NoContent <- clientReRipple rerippleRequest
        pure ()
