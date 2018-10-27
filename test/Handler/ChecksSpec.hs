module Handler.ChecksSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
  describe "getLivenessR" $ do
    it "Should return unit" $ do
      get LivenessR
      statusIs 200
      bodyEquals ""

  describe "getReadinessR" $ do
    it "Should return ready" $ do
      get ReadinessR
      statusIs 200
      bodyEquals "[\"READY\"]"
    