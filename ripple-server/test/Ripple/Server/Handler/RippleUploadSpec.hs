module Ripple.Server.Handler.RippleUploadSpec (spec) where

import Ripple.Client
import Ripple.Server.TestUtils
import Servant.API
import Test.Syd
import Test.Syd.Servant as Servant
import Test.Syd.Validity

spec :: Spec
spec = rippleServerSpec $ do
  it "can upload a ripple" $ \cenv ->
    forAllValid $ \rippleUpload -> do
      NoContent <- testClient cenv $ clientUploadRipple rippleUpload
      pure ()
