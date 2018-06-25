{-# LANGUAGE TemplateHaskell #-}
module GitHashSpec
  ( spec
  ) where

import GitHash
import Test.Hspec

spec :: Spec
spec = do
  it "sanity" $ do
    let gi = $$tGitInfoCwd
    giCommitCount gi `shouldSatisfy` (>= 1)
