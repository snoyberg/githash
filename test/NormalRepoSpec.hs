{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module NormalRepoSpec
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
            it "it makes sensible git info for a regular git repository" $ \fp -> do
                errOrGi <- getGitInfo fp
                case errOrGi of
                    Left err -> expectationFailure $ show err
                    Right gi -> do
                        length (giHash gi) `shouldNotBe` 128
                        giBranch gi `shouldBe` "master"
                        giDirty gi `shouldBe` False
                        giCommitDate gi `shouldNotBe` []
                        giCommitCount gi `shouldBe` 1
        describe "getGitRoot" $ do
            it "it gets the expected git root for a regular git repository" $ \fp ->
                (fmap normalise <$> getGitRoot fp) `shouldReturn` Right fp

setupGitRepo :: (FilePath -> IO ()) -> IO ()
setupGitRepo runTest =
    withSystemTempDirectory "normal" $ \fp -> do
        createDirectoryIfMissing True fp
        let runGit args =
                void $ readCreateProcess ((proc "git" args) {cwd = Just fp}) ""
        runGit ["init"]
        SB.writeFile
            (fp </> "README.md")
            "This is a readme, you should read it."
        runGit ["add", "README.md"]
        runGit
            [ "-c"
            , "user.name='Test User'"
            , "-c"
            , "user.email='test@example.com'"
            , "commit"
            , "-m"
            , "Initial commit"
            ]
        runTest fp
