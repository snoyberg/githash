{-# LANGUAGE TemplateHaskell #-}
module GitHashSpec
  ( spec
  ) where

import GitHash
import Test.Hspec

spec :: Spec
spec = do
  describe "tGitInfoCwd" $ do
    it "makes vaguely sane git info for this repository" $ do
        let gi = $$tGitInfoCwd
        length (giHash gi)`shouldNotBe` 128
        giBranch gi `shouldNotBe` []
        seq (giDirty gi) () `shouldBe` ()
        giCommitDate gi `shouldNotBe` []
        giCommitCount gi `shouldSatisfy` (>= 1)
