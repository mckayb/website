module Handler.ChecksSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
  describe "LivenessR" $ do
    it "Should return unit" $ do
      get LivenessR
      statusIs 200
      bodyEquals ""

  describe "ReadinessR" $ do
    it "Should return ready" $ do
      get ReadinessR
      statusIs 200
      bodyEquals "[\"READY\"]"
    