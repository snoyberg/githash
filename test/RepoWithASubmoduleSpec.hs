{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module RepoWithASubmoduleSpec
    ( spec
    ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as SB
import GitHash
import System.Directory
import System.FilePath
import System.Process
import Test.Hspec
import UnliftIO.Temporary

spec :: Spec
spec =
    around setupGitRepo $ do
        describe "getGitInfo" $ do
            it
                "it makes sensible git info for a both the parent and the child module" $ \(fp1, fp2) -> do
                let sensible fp = do
                        errOrGi <- getGitInfo fp
                        case errOrGi of
                            Left err -> expectationFailure $ show err
                            Right gi -> do
                                length (giHash gi) `shouldNotBe` 128
                                giBranch gi `shouldBe` "master"
                                giDirty gi `shouldBe` False
                                giCommitDate gi `shouldNotBe` []
                                giCommitCount gi `shouldBe` 1
                sensible fp1
                sensible fp2
        describe "getGitRoot" $ do
            it
                "it gets the expected git root for a both the parent and the child module" $ \(fp1, fp2) -> do
                getGitRoot fp1 `shouldReturn` Right fp1
                getGitRoot fp2 `shouldReturn` Right fp2

setupGitRepo :: ((FilePath, FilePath) -> IO ()) -> IO ()
setupGitRepo runTest =
    withSystemTempDirectory "with-submodule" $ \fp -> do
        let fp1 = fp </> "1"
            fp2 = fp </> "2"
        createDirectoryIfMissing True fp1
        createDirectoryIfMissing True fp2
        let runGitIn d args =
                void $ readCreateProcess ((proc "git" args) {cwd = Just d}) ""
            runGit1 = runGitIn fp1
            runGit2 = runGitIn fp2
        runGit1 ["init"]
        runGit2 ["init"]
        SB.writeFile
            (fp2 </> "README.md")
            "This is a readme, you should read it."
        runGit2 ["add", "README.md"]
        runGit2
            [ "-c"
            , "user.name='Test User'"
            , "-c"
            , "user.email='test@example.com'"
            , "commit"
            , "-m"
            , "Initial commit"
            ]
        runGit1 ["submodule", "add", fp2, "2"]
        runGit1
            [ "-c"
            , "user.name='Test User'"
            , "-c"
            , "user.email='test@example.com'"
            , "commit"
            , "-m"
            , "Initial commit"
            ]
        runTest (fp1, fp2)
