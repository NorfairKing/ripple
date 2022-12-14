{-# LANGUAGE RecordWildCards #-}

module Ripple.Server.Handler.ScenarioSpec (spec) where

import Ripple.API
import Ripple.Client
import Ripple.Server.TestUtils
import Test.Syd
import Test.Syd.Servant as Servant
import Test.Syd.Validity

spec :: Spec
spec = rippleServerSpec $ do
  it "can upload a ripple, then find it, and re-ripple it" $ \cenv ->
    forAllValid $ \uploadRippleRequest -> do
      testClient cenv $ do
        uuidUploaded <- clientUploadRipple uploadRippleRequest
        contentsBefore <- clientGetRipple uuidUploaded
        let coordinates = uploadRippleRequestCoordinates uploadRippleRequest
        ripples <- clientListRipples coordinates
        uuidListed <- case ripples of
          [RippleSummary {..}] -> pure rippleSummaryId
          _ -> liftIO $ expectationFailure "Expected exactly one ripple"
        liftIO $ uuidListed `shouldBe` uuidUploaded
        let rerippleRequest =
              ReRippleRequest
                { reRippleRequestCoordinates = coordinates,
                  reRippleRequestId = uuidListed
                }
        uuidAfter <- clientReRipple rerippleRequest
        contentsAfter <- clientGetRipple uuidAfter
        liftIO $ contentsAfter `shouldBe` contentsBefore
