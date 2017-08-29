{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Remotes2LocalMode (run) where

import System.IO (stdout, hFlush)
import Debug.Trace (trace)
import Data.Monoid ((<>))
import CLIFlags (Flags(Remotes2LocalFlags), ignore_tags, ignore_branches, stay_orig_tags, dry_run, force)
import GitCommits
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybe)
import ProcessLib (runCom)

checkFlags :: Flags -> IO ()
checkFlags f@Remotes2LocalFlags{} =
  when (force f && dry_run f) $ error "Options --force and --dry-run are mutually exclusive"

run :: Flags -> IO ()
run flags = do
  checkFlags flags
  commits' <- getLogCommitsMap
  case commits' of
    Left x -> error $ "Can not realize info about git commits: " ++ show x
    Right commits -> runReaderT (runStateT process initialState) (makeConf flags commits)

  return ()

data Conf = Conf { getFlags :: Flags
                 , getCommits :: GitCommitsMap
                 } deriving (Show)
type TagSubstitutions = Map.Map RefName (Set.Set RefName)
type BranchSubstitutions = Set.Set (RepoName, RefName)
type MetRepoNames = Set.Set RepoName
type PassedCommits = Set.Set GitHash
data TraverseState = TraverseState
                     {
                       metRepoNames :: MetRepoNames
                     , passedCommits :: PassedCommits
                     , tagSubs :: TagSubstitutions
                     , branchSubs :: BranchSubstitutions
                     } deriving (Show)

makeConf = Conf
initialState = TraverseState {metRepoNames = Set.empty, passedCommits = Set.empty, tagSubs = Map.empty, branchSubs = Set.empty}

process :: (MonadReader Conf m, MonadState TraverseState m, MonadIO m) => m ()
process = do
  getTagSubstitutions
  getBranchSubstitutions
  
  fireUp <- mustRun
  when fireUp $ do
    applyTagSubstitutions
    applyBranchSubstitutions

  where
    askUserConfirmation :: (MonadReader Conf m, MonadState TraverseState m, MonadIO m) => m Bool
    askUserConfirmation = do
      force' <- force . getFlags <$> ask
      if force'
        then return True
        else do
          stayOrigTags <- stay_orig_tags . getFlags <$> ask
          tagSubsts <- tagSubs <$> get
          branchSubsCount <- Set.size . branchSubs <$> get
          let
            tagCreateCount = Map.foldl (\s v -> s + Set.size v) 0 tagSubsts
            tagDeleteCount = Map.size tagSubsts

          liftIO $ putStrLn "Proposed changes:"
          liftIO . putStrLn $ "  " <> show tagCreateCount <> " tag(s) will be created"
          unless stayOrigTags $ liftIO . putStrLn $ "  " <> show tagDeleteCount <> " tag(s) will be deleted"
          liftIO . putStrLn $ "  " <> show branchSubsCount <> " branch(es) will be created"
          (\x -> x == "Y" || x == "y") <$> liftIO (putStr "Are you sure? [Y/N]: " >> hFlush stdout >> getLine)
    mustRun :: (MonadReader Conf m, MonadState TraverseState m, MonadIO m) => m Bool
    mustRun = do
      isProposedChangesAbsent <- (&&) <$> (Map.null . tagSubs <$> get) <*> (Set.null . branchSubs <$> get)
      dryRun <- dry_run . getFlags <$> ask
      if isProposedChangesAbsent then return False
      else if dryRun then return True
           else askUserConfirmation

applyTagSubstitutions :: (MonadReader Conf m, MonadState TraverseState m, MonadIO m) => m ()
applyTagSubstitutions =
  gets tagSubs >>= mapM_ processOneTagSubstitution . fmap (fmap Set.toList) . Map.toList
  where
    processOneTagSubstitution :: (MonadReader Conf m, MonadIO m) => (RefName, [RefName]) -> m ()
    processOneTagSubstitution (oldName, newNames) = do
      dryRun <- dry_run . getFlags <$> ask
      mapM_ (\newName -> liftIO . runCom dryRun $ "git tag " <> newName <> " " <> oldName) newNames
      stay_orig_tags' <- stay_orig_tags . getFlags <$> ask
      unless stay_orig_tags' (liftIO $ runCom dryRun $ "git tag --delete " <> oldName)

getTagSubstitutions :: (MonadReader Conf m, MonadState TraverseState m) => m ()
getTagSubstitutions = do
  ignoreTags <- ignore_tags . getFlags <$> ask
  unless ignoreTags $ do
    topCommits <- getTopCommits . getCommits <$> ask
    mapM_ traverseFromTagWithClearRepoNames topCommits
  where
    traverseFromTagWithClearRepoNames commit = do
      st <- get
      put st{metRepoNames = Set.empty, passedCommits = Set.empty}
      traverseFromTag commit

isSuitableRepoName :: RepoName -> Bool
isSuitableRepoName = (/=) "origin"


getRemoteRepoNames :: GitCommit -> [RepoName]
getRemoteRepoNames =
  filter isSuitableRepoName . fmap fst . getRemoteBranches

traverseFromTag :: (MonadReader Conf m, MonadState TraverseState m) => GitCommit -> m ()
traverseFromTag commit = do
  st@TraverseState{passedCommits=passedCommits'} <- get

  when (Set.notMember (hash commit) passedCommits') $ do
    put st{passedCommits=Set.union passedCommits' (Set.singleton $ hash commit) }
    commitsMap <- getCommits <$> ask
    addRepoNames
    applyTags
    mapM_ traverseFromTag (hashesToCommits commitsMap $ parents commit)
  where
    addRepoNames :: (MonadState TraverseState m) => m ()
    addRepoNames = do
      st <- get
      put st{ metRepoNames = Set.union (metRepoNames st) (Set.fromList $ getRemoteRepoNames commit) }

    applyTags :: (MonadReader Conf m, MonadState TraverseState m) => m ()
    applyTags = do
      allRepoNames <- Set.toList . Set.fromList . concatMap getRemoteRepoNames . toCommits . getCommits <$> ask
      TraverseState{metRepoNames = metRepoNames'} <- get
      let
        existingTags = getTags commit
        isTagAlreadyExists tagName = tagName `elem` existingTags
        metRepoNamesList = Set.toList metRepoNames'
        getNewTagName repoName originTagName = repoName <> "/" <> originTagName
        isPrefixedWithAnyRepoName tagName = any (`T.isPrefixOf` (tagName <> "/")) allRepoNames
      mapM_ addTagSubstitution
        [ (e, getNewTagName r e) | r <- metRepoNamesList, e <- existingTags
                                 , not ( isTagAlreadyExists (getNewTagName r e) || isPrefixedWithAnyRepoName e )]
      unless (Set.null metRepoNames') $
        mapM_ addTagForDeletion . filter (not . isPrefixedWithAnyRepoName) $ existingTags
      where
        addTagSubstitution (origName, newName) = do
          st@TraverseState{tagSubs = tagSubs'} <- get
          put st{tagSubs = Map.alter combine origName tagSubs'}
          where
            combine (Just x) = Just $ Set.union x $ Set.singleton newName
            combine Nothing = Just $ Set.singleton newName

        addTagForDeletion origName = do
          st@TraverseState{tagSubs = tagSubs'} <- get
          put st{tagSubs = Map.alter combine origName tagSubs'}
          where
            combine (Just x) = Just x
            combine Nothing = Just Set.empty


newBranchName repoName refName = repoName <> "/" <> refName 

getBranchSubstitutions :: (MonadReader Conf m, MonadState TraverseState m) => m ()
getBranchSubstitutions = do
  ignoreBranches <- ignore_branches . getFlags <$> ask
  unless ignoreBranches $ do
    commits <- toCommits . getCommits <$> ask
    st <- get
    put st{ branchSubs = Set.fromList . concatMap oneCommit $ commits }
  where
    getFromRef (GitRemoteBranch repoName refName) = Just (repoName, refName)
    getFromRef _ = Nothing

    oneCommit :: GitCommit -> [(RepoName, RefName)]
    oneCommit commit =
      filter checkRepoName . filter notExists . mapMaybe getFromRef . refs $ commit
      where
        notExists :: (RepoName, RefName) -> Bool
        notExists (repoName, refName) =
          newBranchName repoName refName `notElem` getBranches commit

        checkRepoName (repoName, _) = isSuitableRepoName repoName
          

applyBranchSubstitutions :: (MonadReader Conf m, MonadState TraverseState m, MonadIO m) => m ()
applyBranchSubstitutions =
  Set.toList . branchSubs <$> get >>= mapM_ applyOneBranch
  where
    applyOneBranch :: (MonadReader Conf m, MonadIO m) => (RepoName, RefName) -> m ()
    applyOneBranch (repoName, refName) = do
      dryRun <- dry_run . getFlags <$> ask
      liftIO . runCom dryRun $ "git branch --track " <> newBranchName' <> " " <> "remotes/" <> newBranchName'
      where
        newBranchName' = newBranchName repoName refName
