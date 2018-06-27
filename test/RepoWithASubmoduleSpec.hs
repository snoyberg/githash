{-# LANGUAGE TemplateHaskell #-}

module RepoWithASubmoduleSpec
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
        it
            "it makes sensible git info for a both the parent and the child module" $ \(fp1, fp2) -> do
            let sensible fp = do
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
            sensible fp1
            sensible fp2

setupGitRepo :: ((FilePath, FilePath) -> IO ()) -> IO ()
setupGitRepo runTest = do
    let fp = "/tmp/repo-with-submodule"
        fp1 = fp </> "1"
        fp2 = fp </> "2"
    createDirectoryIfMissing True fp1
    createDirectoryIfMissing True fp2
    let runGitIn d cmd =
            void $ readCreateProcess ((shell $ "git " ++ cmd) {cwd = Just d}) ""
        runGit1 = runGitIn fp1
        runGit2 = runGitIn fp2
    runGit1 "init"
    runGit2 "init"
    writeFile (fp2 </> "README.md") "This is a readme, you should read it."
    runGit2 "add README.md"
    runGit2 "commit -m 'Initial commit'"
    runGit1 $ unwords ["submodule add", fp2, "2"]
    runGit1 "commit -m 'Initial commit'"
    runTest (fp1, fp2) `finally` removeDirectoryRecursive fp
