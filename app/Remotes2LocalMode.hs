{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Remotes2LocalMode (run) where

import System.IO (stdout, hFlush)
import Data.Monoid ((<>))
import CLIFlags (Flags(Remotes2LocalFlags), tag, no_tag, branch, no_branch, repo, no_repo, tag_del, no_tag_del, dry_run, force)
import GitCommits
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import ProcessLib (runCom)
import Text.Regex.TDFA

checkFlags :: Flags -> IO ()
checkFlags f@Remotes2LocalFlags{} =
  return ()

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
type TagDeletions = Set.Set RefName
type BranchSubstitutions = Set.Set (RepoName, RefName)
type MetRepoNames = Set.Set RepoName
type PassedCommits = Set.Set GitHash
data TraverseState = TraverseState
                     {
                       metRepoNames :: MetRepoNames
                     , passedCommits :: PassedCommits
                     , tagSubs :: TagSubstitutions
                     , tagDels :: TagDeletions
                     , branchSubs :: BranchSubstitutions
                     } deriving (Show)

makeConf = Conf
initialState = TraverseState {metRepoNames = Set.empty, passedCommits = Set.empty, tagSubs = Map.empty, tagDels = Set.empty, branchSubs = Set.empty}

hasProposedChanges :: (MonadState TraverseState m) => m Bool
hasProposedChanges = do
  s <- get
  return . any ($ s) $
    [ not . Map.null . tagSubs
    , not . Set.null . tagDels
    , not . Set.null . branchSubs
    ]

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
          tagCreateCount <- Map.foldl (\s v -> s + Set.size v) 0 . tagSubs <$> get
          tagDeleteCount <- Set.size . tagDels <$> get
          branchSubsCount <- Set.size . branchSubs <$> get

          if (tagCreateCount + tagDeleteCount + branchSubsCount) == 0
            then
              return True
            else do
              liftIO $ putStrLn "Proposed changes:"
              liftIO . putStrLn $ "  " <> show tagCreateCount <> " tag(s) will be created"
              liftIO . putStrLn $ "  " <> show tagDeleteCount <> " tag(s) will be deleted"
              liftIO . putStrLn $ "  " <> show branchSubsCount <> " branch(es) will be created"
              (\x -> x == "Y" || x == "y") <$> liftIO (putStr "Are you sure? [Y/N]: " >> hFlush stdout >> getLine)
    mustRun :: (MonadReader Conf m, MonadState TraverseState m, MonadIO m) => m Bool
    mustRun = do
      hasProposedChanges' <- hasProposedChanges
      dryRun <- dry_run . getFlags <$> ask
      if not hasProposedChanges'
        then return False
        else askUserConfirmation

applyTagSubstitutions :: (MonadReader Conf m, MonadState TraverseState m, MonadIO m) => m ()
applyTagSubstitutions = do
  gets tagSubs >>= mapM_ processOneTagSubstitution . fmap (fmap Set.toList) . Map.toList
  gets tagDels >>= mapM_ processOneTagDeletion . Set.toList
  where
    processOneTagSubstitution :: (MonadReader Conf m, MonadIO m) => (RefName, [RefName]) -> m ()
    processOneTagSubstitution (oldName, newNames) = do
      dryRun <- dry_run . getFlags <$> ask
      mapM_ (\newName -> liftIO . runCom dryRun $ "git tag " <> newName <> " " <> oldName) newNames
    processOneTagDeletion :: (MonadReader Conf m, MonadIO m) => RefName -> m ()
    processOneTagDeletion oldName = do
      dryRun <- dry_run . getFlags <$> ask
      liftIO $ runCom dryRun $ "git tag --delete " <> oldName

getTagSubstitutions :: (MonadReader Conf m, MonadState TraverseState m) => m ()
getTagSubstitutions = do
  topCommits <- getTopCommits . getCommits <$> ask
  mapM_ traverseForTagWithClearRepoNames topCommits
  where
    traverseForTagWithClearRepoNames commit = do
      st <- get
      put st{metRepoNames = Set.empty, passedCommits = Set.empty}
      traverseForTag commit

isSuitableRepoName :: RepoName -> Bool
isSuitableRepoName = (/=) "origin"

getRemoteRepoNames :: (RepoName -> Bool) -> GitCommit -> [RepoName]
getRemoteRepoNames check =
  filter check . fmap fst . getRemoteBranches

traverseForTag :: (MonadReader Conf m, MonadState TraverseState m) => GitCommit -> m ()
traverseForTag commit = do
  st@TraverseState{passedCommits=passedCommits'} <- get

  when (Set.notMember (hash commit) passedCommits') $ do
    put st{passedCommits=Set.union passedCommits' (Set.singleton $ hash commit) }
    commitsMap <- getCommits <$> ask
    addRepoNames
    applyTags
    mapM_ traverseForTag (hashesToCommits commitsMap $ parents commit)
  where
    addRepoNames :: (MonadReader Conf m, MonadState TraverseState m) => m ()
    addRepoNames = do
      st <- get
      conf <- ask
      put st{ metRepoNames = Set.union (metRepoNames st) (Set.fromList $ getRemoteRepoNames isSuitableRepoName commit) }

    applyTags :: (MonadReader Conf m, MonadState TraverseState m) => m ()
    applyTags = do
      conf <- ask
      TraverseState{metRepoNames = metRepoNames'} <- get
      let
        existingTags = getTags commit
        selectedTags4Create = filter (not . isPrefixedWithAnyRepoName) . filter (isMatchTag conf) $ existingTags
        selectedTags4Deletion = filter (not . isPrefixedWithAnyRepoName) . filter (isMatchTagDel conf) $ existingTags
        isTagAlreadyExists tagName = tagName `elem` existingTags
        selectedRepoNamesList = filter (isMatchRepoName conf) . Set.toList $ metRepoNames'
        getNewTagName repoName originTagName = repoName <> "/" <> originTagName
        allRepoNames = Set.toList . Set.fromList . concatMap (getRemoteRepoNames isSuitableRepoName) . toCommits . getCommits $ conf
        isPrefixedWithAnyRepoName tagName = any (`T.isPrefixOf` (tagName <> "/")) allRepoNames
      mapM_ addTagSubstitution
        [ (e, getNewTagName r e) | r <- selectedRepoNamesList, e <- selectedTags4Create
                                 , not (isTagAlreadyExists (getNewTagName r e))]
      unless (Set.null metRepoNames') $
        mapM_ addTagForDeletion selectedTags4Deletion
      where
        addTagSubstitution (origName, newName) = do
          st@TraverseState{tagSubs = tagSubs'} <- get
          put st{tagSubs = Map.alter combine origName tagSubs'}
          where
            combine (Just x) = Just $ Set.union x $ Set.singleton newName
            combine Nothing = Just $ Set.singleton newName

        addTagForDeletion origName = do
          st@TraverseState{tagDels = tagDels'} <- get
          put st{tagDels = Set.union tagDels' $ Set.singleton origName}
          where
            combine (Just x) = Just x
            combine Nothing = Just Set.empty


newBranchName repoName refName = repoName <> "/" <> refName

getBranchSubstitutions :: (MonadReader Conf m, MonadState TraverseState m) => m ()
getBranchSubstitutions = do
  ignoreBranches <- elem ".*" . no_branch . getFlags <$> ask
  unless ignoreBranches $ do
    st <- get
    conf <- ask
    put st{ branchSubs = Set.fromList . concatMap (oneCommit conf) . toCommits . getCommits $ conf }
  where
    getFromRef (GitRemoteBranch repoName refName) = Just (repoName, refName)
    getFromRef _ = Nothing

    oneCommit :: Conf -> GitCommit -> [(RepoName, RefName)]
    oneCommit conf commit =
      filter (isMatchBranch conf . snd) . filter (checkRepoName conf) . filter notExists . mapMaybe getFromRef . refs $ commit
      where
        notExists :: (RepoName, RefName) -> Bool
        notExists (repoName, refName) =
          newBranchName repoName refName `notElem` getBranches commit

        checkRepoName conf (repoName, _) = isMatchRepoName conf repoName


applyBranchSubstitutions :: (MonadReader Conf m, MonadState TraverseState m, MonadIO m) => m ()
applyBranchSubstitutions =
  Set.toList . branchSubs <$> get >>= mapM_ applyOneBranch
  where
    applyOneBranch :: (MonadReader Conf m, MonadIO m) => (RepoName, RefName) -> m ()
    applyOneBranch (repoName, refName) = do
      dryRun <- dry_run . getFlags <$> ask
      liftIO . runCom dryRun $ "git branch --track " <> newBranchName' <> " --force " <> "remotes/" <> newBranchName'
      where
        newBranchName' = newBranchName repoName refName

isMatch :: [String] -> [String] -> T.Text -> Bool
isMatch positive negative name =
  let
    checkRegex regex = T.unpack name =~ regex :: Bool
    isPositive = null positive || any checkRegex positive
    isNegative = any checkRegex negative
  in isPositive && not isNegative

isMatchRepoName :: Conf -> RepoName -> Bool
isMatchRepoName conf repoName =
  let
    positive = repo . getFlags $ conf
    negative = no_repo . getFlags $ conf
    isOriginRepo = not . isSuitableRepoName $ repoName
  in not isOriginRepo && (isMatch positive negative repoName)

isMatchTag :: Conf -> RefName -> Bool
isMatchTag conf tagName =
  let
    positive = tag . getFlags $ conf
    negative = no_tag . getFlags $ conf
  in
    isMatch positive negative tagName

isMatchTagDel :: Conf -> RefName -> Bool
isMatchTagDel conf tagName =
  let
    positive1 = tag_del . getFlags $ conf
    negative1 = no_tag_del . getFlags $ conf
    assigned = not (null positive1 && null negative1)
    positive2 = tag . getFlags $ conf
    negative2 = no_tag . getFlags $ conf
    positive = if assigned then positive1 else positive2
    negative = if assigned then negative1 else negative2
  in
    isMatch positive negative tagName

isMatchBranch :: Conf -> RefName -> Bool
isMatchBranch conf branchName =
  let
    positive = branch . getFlags $ conf
    negative = no_branch . getFlags $ conf
  in
    isMatch positive negative branchName
