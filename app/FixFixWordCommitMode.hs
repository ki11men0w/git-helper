{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module FixFixWordCommitMode (run) where

import GitCommits
import CLIFlags hiding (getFlags)
import ProcessLib (runCom)

import Control.Monad (when, unless)
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)

import Data.Monoid ((<>))
import System.Exit (ExitCode(..))
import System.Process
import Data.List (partition, sort, intersect)
import System.IO (stdout, hFlush)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Either (isRight)
import Control.Applicative (many)

import Data.Version (showVersion)

import qualified Data.Text as T
import Data.Text (pack, unpack)
import Data.Attoparsec.Text.Lazy


checkFlags :: Flags -> IO ()
checkFlags flags@FixFixWordCommitFlags{} =
  let intersectBranches = intersect (include_branch flags) (exclude_branch flags)
  in unless (null intersectBranches) $
    error $ "Branches must be ether in --include-branch or --exclude-branch. These branches are in both lists: " <> show intersectBranches


data Conf = Conf { getFlags :: Flags
                 , getCommits :: GitCommitsMap
                 } deriving (Show)

run :: Flags -> IO ()
run flags = do
  checkFlags flags

  result <- getLogCommitsMap
  case result of
    Left errMsg -> error errMsg
    Right commitsMap -> runReaderT process Conf{ getFlags = flags, getCommits = commitsMap }

process :: (MonadReader Conf m, MonadIO m) => m ()
process = do
  flags <- asks getFlags
  commitsMap <- asks getCommits
  let
    branchesToInclude = filter (`notElem` neverCorrectableRefNames) . map pack $ include_branch flags
    branchesToExclude = map pack $ exclude_branch flags
    isSpecialBranchName :: RefName -> Bool
    isSpecialBranchName = isSpecialBranchEx branchesToInclude branchesToExclude
    isMergedCommit' = isMergedCommit commitsMap
    correctThisBranch (B2R refName _ commit revert2)
      | refName `elem` branchesToExclude = False
      | refName `elem` branchesToInclude = True
      | not $ isMergedCommit' revert2 = False
      | isSpecialBranchName refName = False
      | otherwise = True

    remoteBranches = getRemoteBranchesToRevert commitsMap
    (branches4Correction, branchesStayUnchanged) = partition correctThisBranch remoteBranches

  unless (null branches4Correction) $ do
    fireUp <- askUser branches4Correction
    when fireUp $ if delete flags
                  then do
                    deleteBranches (map b2rRefName branches4Correction)
                    liftIO . putStrLn $ (show . length $ branches4Correction) <> " branch(es) was successfully deleted."
                  else do
                    revertBranches branches4Correction
                    liftIO . putStrLn $ (show . length $ branches4Correction) <> " branch(es) was successfully reverted."
  unless (null branchesStayUnchanged) $ do
    liftIO $ putStrLn "And these branches were kept untouched:"
    liftIO $ mapM_ print . sort . map b2rRefName $ branchesStayUnchanged
  where
    askUser branches4Correction = do
      delete' <- delete . getFlags <$> ask
      if delete'
        then liftIO $ putStrLn "These branches will be deleted from the remote repository 'origin':"
        else liftIO $ putStrLn "These branches will be reverted one/several commit(s) back in the remote repository 'origin':"
      liftIO . mapM_ print . sort . map b2rRefName $ branches4Correction
      (\x -> x == "Y" || x == "y") <$> liftIO (putStr "Are you sure? [Y/N]: " >> hFlush stdout >> getLine)
  

isFixWordCommit :: GitCommit -> Bool
isFixWordCommit = (=="fix word commit") . fromMaybe "" . message

isSpecialBranch :: RefName -> Bool
isSpecialBranch =
  isRight . parseOnly parseSpecialBranchName
  where
    parseSpecialBranchName :: Parser ()
    parseSpecialBranchName =
      choice . map (*> endOfInput) $
      [
        exactNames     *> pure ()
      , officialPrefix *> pure ()
      , officialSuffix *> pure ()
      , officialInfix  *> pure ()
      , fullVersion    *> pure ()
      , majorVersion   *> pure ()
      , productVersion *> pure ()
      , oldPatch       *> pure ()
      ]
      where
        dot = char '.'
        officialBranchMarkers = ["master","develop","release"]

        exactNames = choice . map string $ officialBranchMarkers
        officialPrefix = choice . map (\x -> string x *> char '/' *> many anyChar) $ officialBranchMarkers
        officialSuffix = choice . map (\x -> manyTill anyChar (char '/' *> string x)) $ officialBranchMarkers
        officialInfix = officialSuffix *> char '/' *> many1 anyChar
        fullVersion = count 3 digit *> dot *> count 2 digit
        majorVersion = count 3 digit
        productVersion = many1 digit *> dot *> many1 digit *> dot *> many1 digit
        oldPatch = string "Patches_" *> majorVersion

isSpecialBranchEx :: [RefName] -> [RefName] -> RefName -> Bool
isSpecialBranchEx includes excludes branchName =
  let isCommonSpecialBranch = isSpecialBranch branchName
      isInclude = branchName `elem` includes
      isExclude = branchName `elem` excludes
  in isExclude || (not isInclude && isCommonSpecialBranch)

isGitRemoteBranch :: GitReference -> Bool
isGitRemoteBranch (GitRemoteBranch "origin" _) = True
isGitRemoteBranch _ = False

remoteBranchName :: GitReference -> Maybe RefName
remoteBranchName (GitRemoteBranch "origin" branch) = Just branch
remoteBranchName _ = Nothing

revertBranches :: (MonadReader Conf m, MonadIO m) => [Branch2Revert] -> m ()
revertBranches =
  mapM_ revertBranch
  where
    revertBranch (B2R refName depth _ _) = do
      dryRun <- dry_run . getFlags <$> ask
      liftIO $ runCom dryRun $ "git push -f origin origin/" <> refName <> pack (replicate depth '~') <> ":" <> refName

deleteBranches :: (MonadReader Conf m, MonadIO m) => [RefName] -> m ()
deleteBranches =
  mapM_ deleteBranch
  where
    deleteBranch refName = do
      dryRun <- dry_run . getFlags <$> ask
      liftIO $ runCom dryRun $ "git push --delete origin " <> refName

-- | Возвращает количество коммитов на которое нужно отступить назад от указанного
-- коммита 'commit', что-бы вернутся к комиту содержащему состояние репозитория
-- до применения скрипта по корректировке документации.
getRevertCount :: GitCommitsMap -> GitCommit -> Maybe (GitCommit, Int)
getRevertCount commitsMap commit =
  findNext commit 1
  where
    findNext commit depth =
      let parents' = hashesToCommits commitsMap (parents commit)
      in
        case parents' of
          [c] -> if isFixWordCommit c
                 then findNext c (succ depth)
                 else Just (c, depth)
          _ -> Nothing 
  

getSutableFixTheWordCommits :: GitCommitsMap -> [GitCommit]
getSutableFixTheWordCommits commitsMap =
  let commits = toCommits commitsMap
  in filter hasRemoteBranches . filter isFixWordCommit $ commits
  where
    hasRemoteBranches = any isGitRemoteBranch . refs

getCommitsToRevert :: GitCommitsMap -> [(GitCommit, (GitCommit, Int))]
getCommitsToRevert commitsMap =
  mapMaybe withRevertCount . getSutableFixTheWordCommits $ commitsMap
  where
    getSutableFixTheWordCommits' = getSutableFixTheWordCommits commitsMap
    getRevertCount' = getRevertCount commitsMap

    withRevertCount :: GitCommit -> Maybe (GitCommit, (GitCommit, Int))
    withRevertCount commit = (commit,) <$> getRevertCount' commit

data Branch2Revert = B2R { b2rRefName :: RefName
                         , b2rDepth :: Int
                         , b2rCommit :: GitCommit
                         , b2rRevertTo :: GitCommit
                         }

getRemoteBranchesToRevert :: GitCommitsMap -> [Branch2Revert]
getRemoteBranchesToRevert =
  concatMap toRemoteBranches . getCommitsToRevert
  where
    toRemoteBranches (commit, (toCommit, depth)) =
      map (\x -> B2R{b2rRefName=x, b2rDepth=depth, b2rCommit=commit, b2rRevertTo=toCommit}) . filter (`notElem` neverCorrectableRefNames) . mapMaybe remoteBranchName . refs $ commit


isMergedCommit :: GitCommitsMap -> GitCommit -> Bool
isMergedCommit commitsMap commit =
  any isMergedCommit' $ hashesToCommits commitsMap $ childrens commit
  where
    isMergedCommit' :: GitCommit -> Bool
    isMergedCommit' commit =
      any isGitRemoteBranch (refs commit)
      ||
      any isMergedCommit' (hashesToCommits commitsMap $ childrens commit)

neverCorrectableRefNames = ["HEAD"]
