{-# LANGUAGE TemplateHaskell #-}

module NormalRepoSpec
    ( spec
    ) where

import Control.Exception
import Control.Monad
import GitHash
import System.Directory
import System.FilePath
import System.Process
import Test.Hspec

spec :: Spec
spec =
    around setupGitRepo $
    describe "getGitInfo" $ do
        it "it makes sensible git info for a regular git repository" $ \fp -> do
            errOrGi <- getGitInfo fp
            case errOrGi of
                Left err -> expectationFailure $ show err
                Right gi -> do
                    getGitRoot fp `shouldReturn` Right fp
                    length (giHash gi) `shouldNotBe` 128
                    giBranch gi `shouldBe` "master"
                    giDirty gi `shouldBe` False
                    giCommitDate gi `shouldNotBe` []
                    giCommitCount gi `shouldBe` 1

setupGitRepo :: (FilePath -> IO ()) -> IO ()
setupGitRepo runTest = do
    let fp = "/tmp/repo"
    createDirectoryIfMissing True fp
    let runGit cmd =
            void $
            readCreateProcess ((shell $ "git " ++ cmd) {cwd = Just fp}) ""
    runGit "init"
    writeFile (fp </> "README.md") "This is a readme, you should read it."
    runGit "add README.md"
    runGit "commit -m 'Initial commit'"
    runTest fp `finally` removeDirectoryRecursive fp
