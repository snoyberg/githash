{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2018 Michael Snoyman, 2015 Adam C. Foltzer
-- License     :  BSD3
-- Maintainer  :  michael@snoyman.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Some handy Template Haskell splices for including the current git
-- hash and branch in the code of your project. Useful for including
-- in panic messages, @--version@ output, or diagnostic info for more
-- informative bug reports.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import GitHash
-- >
-- > panic :: String -> a
-- > panic msg = error panicMsg
-- >   where panicMsg =
-- >           concat [ "[panic ", giBranch gi, "@", giHash gi
-- >                  , " (", giCommitDate gi, ")"
-- >                  , " (", show (giCommitCount gi), " commits in HEAD)"
-- >                  , dirty, "] ", msg ]
-- >         dirty | giDirty gi = " (uncommitted files present)"
-- >               | otherwise   = ""
-- >         gi = $$tGitInfoCwd
-- >
-- > main = panic "oh no!"
--
-- > % stack runghc Example.hs
-- > Example.hs: [panic master@2ae047ba5e4a6f0f3e705a43615363ac006099c1 (Mon Jan 11 11:50:59 2016 -0800) (14 commits in HEAD) (uncommitted files present)] oh no!
--
-- WARNING: None of this will work in a git repository without any commits.
--
-- @since 0.1.0.0
module GitHash
  ( -- * Types
    GitInfo
  , GitHashException (..)
    -- ** Getters
  , giHash
  , giBranch
  , giDirty
  , giCommitDate
  , giCommitCount
  , giCommitMessage
    -- * Creators
  , getGitInfo
  , getGitRoot
    -- * Template Haskell
  , tGitInfo
  , tGitInfoCwd
  , tGitInfoTry
  , tGitInfoCwdTry
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Maybe
import Data.Typeable (Typeable)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.Process
import Text.Read (readMaybe)

-- | Various pieces of information about a Git repository.
--
-- @since 0.1.0.0
data GitInfo = GitInfo
  { _giHash :: !String
  , _giBranch :: !String
  , _giDirty :: !Bool
  , _giCommitDate :: !String
  , _giCommitCount :: !Int
  , _giFiles :: ![FilePath]
  , _giCommitMessage :: !String
  }
  deriving (Lift, Show)

-- | The hash of the most recent commit.
--
-- @since 0.1.0.0
giHash :: GitInfo -> String
giHash = _giHash

-- | The hash of the most recent commit.
--
-- @since 0.1.0.0
giBranch :: GitInfo -> String
giBranch = _giBranch

giDirty :: GitInfo -> Bool
giDirty = _giDirty

giCommitDate :: GitInfo -> String
giCommitDate = _giCommitDate

giCommitCount :: GitInfo -> Int
giCommitCount = _giCommitCount

-- | The message of the most recent commit.
--
-- @since 0.1.1.0
giCommitMessage :: GitInfo -> String
giCommitMessage = _giCommitMessage

-- | Get the 'GitInfo' for the given root directory. Root directory
-- should be the directory containing the @.git@ directory.
--
-- @since 0.1.0.0
getGitInfo :: FilePath -> IO (Either GitHashException GitInfo)
getGitInfo root = try $ do
  -- a lot of bookkeeping to record the right dependencies
  let hd         = root </> ".git" </> "HEAD"
      index      = root </> ".git" </> "index"
      packedRefs = root </> ".git" </> "packed-refs"
  ehdRef <- try $ B.readFile hd
  files1 <-
    case ehdRef of
      Left e
        | isDoesNotExistError e -> return []
        | otherwise -> throwIO $ GHECouldn'tReadFile hd e
      Right hdRef -> do
        -- the HEAD file either contains the hash of a detached head
        -- or a pointer to the file that contains the hash of the head
        case B.splitAt 5 hdRef of
          -- pointer to ref
          ("ref: ", relRef) -> do
            let ref = root </> ".git" </> B8.unpack relRef
            refExists <- doesFileExist ref
            return $ if refExists then [ref] else []
          -- detached head
          _hash -> return [hd]
  -- add the index if it exists to set the dirty flag
  indexExists <- doesFileExist index
  let files2 = if indexExists then [index] else []
  -- if the refs have been packed, the info we're looking for
  -- might be in that file rather than the one-file-per-ref case
  -- handled above
  packedExists <- doesFileExist packedRefs
  let files3 = if packedExists then [packedRefs] else []

      _giFiles = concat [files1, files2, files3]
      run args = do
        eres <- runGit root args
        case eres of
          Left e -> throwIO e
          Right str -> return $ takeWhile (/= '\n') str

  _giHash <- run ["rev-parse", "HEAD"]
  _giBranch <- run ["rev-parse", "--abbrev-ref", "HEAD"]

  dirtyString <- run ["status", "--porcelain"]
  let _giDirty = not $ null (dirtyString :: String)

  commitCount <- run ["rev-list", "HEAD", "--count"]
  _giCommitCount <-
    case readMaybe commitCount of
      Nothing -> throwIO $ GHEInvalidCommitCount root commitCount
      Just x -> return x

  _giCommitDate <- run ["log", "HEAD", "-1", "--format=%cd"]

  _giCommitMessage <- run ["log", "-1", "--pretty=%B"]

  return GitInfo {..}

-- | Get the root directory of the Git repo containing the given file
-- path.
--
-- @since 0.1.0.0
getGitRoot :: FilePath -> IO (Either GitHashException FilePath)
getGitRoot dir = fmap (normalise . takeWhile (/= '\n')) `fmap` (runGit dir ["rev-parse", "--show-toplevel"])

runGit root args = do
  let cp = (proc "git" args) { cwd = Just root }
  eres <- try $ readCreateProcessWithExitCode cp ""
  return $ case eres of
    Left e -> Left $ GHEGitRunException root args e
    Right (ExitSuccess, out, _) -> Right out
    Right (ec@ExitFailure{}, out, err) -> Left $ GHEGitRunFailed root args ec out err

-- | Exceptions which can occur when using this library's functions.
--
-- @since 0.1.0.0
data GitHashException
  = GHECouldn'tReadFile !FilePath !IOException
  | GHEInvalidCommitCount !FilePath !String
  | GHEGitRunFailed !FilePath ![String] !ExitCode !String !String
  | GHEGitRunException !FilePath ![String] !IOException
  deriving (Show, Eq, Typeable)
instance Exception GitHashException

-- | Load up the 'GitInfo' value at compile time for the given
-- directory. Compilation fails if no info is available.
--
-- @since 0.1.0.0
tGitInfo :: FilePath -> Q (TExp GitInfo)
tGitInfo fp = unsafeTExpCoerce $ do
  gi <- runIO $
    getGitRoot fp >>=
    either throwIO return >>=
    getGitInfo >>=
    either throwIO return
  mapM_ addDependentFile (_giFiles gi)
  lift (gi :: GitInfo) -- adding type sig to make the unsafe look slightly better

-- | Try to load up the 'GitInfo' value at compile time for the given
-- directory.
--
-- @since 0.1.2.0
tGitInfoTry :: FilePath -> Q (TExp (Either String GitInfo))
tGitInfoTry fp = unsafeTExpCoerce $ do
  egi <- runIO $ do
    eroot <- getGitRoot fp
    case eroot of
      Left e -> return $ Left $ show e
      Right root -> do
        einfo <- getGitInfo root
        case einfo of
          Left e -> return $ Left $ show e
          Right info -> return $ Right info
  case egi of
    Left _ -> return ()
    Right gi -> mapM_ addDependentFile (_giFiles gi)
  lift (egi :: Either String GitInfo) -- adding type sig to make the unsafe look slightly better

-- | Load up the 'GitInfo' value at compile time for the current
-- working directory.
--
-- @since 0.1.0.0
tGitInfoCwd :: Q (TExp GitInfo)
tGitInfoCwd = tGitInfo "."

-- | Try to load up the 'GitInfo' value at compile time for the current
-- working directory.
--
-- @since 0.1.2.0
tGitInfoCwdTry :: Q (TExp (Either String GitInfo))
tGitInfoCwdTry = tGitInfoTry "."
