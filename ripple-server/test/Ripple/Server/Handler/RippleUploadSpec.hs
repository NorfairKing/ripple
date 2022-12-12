module Ripple.Server.Handler.RippleUploadSpec (spec) where

import Ripple.API
import Ripple.Client
import Ripple.Server.TestUtils
import Test.Syd
import Test.Syd.Servant as Servant
import Test.Syd.Validity

spec :: Spec
spec = rippleServerSpec $ do
  it "can roundtrip-upload a ripple" $ \cenv ->
    forAllValid $ \rippleUpload -> do
      testClient cenv $ do
        uuid <- clientUploadRipple rippleUpload
        content <- clientGetRipple uuid
        liftIO $ content `shouldBe` uploadRippleRequestFileContents rippleUpload
