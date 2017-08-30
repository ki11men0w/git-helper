{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE DeriveDataTypeable #-}
module CLIFlags (Flags(..), getFlags) where

import Paths_git_helper (version)
import Data.Version (showVersion)
import System.Environment (getProgName)
import System.Console.CmdArgs
import Data.List (intercalate)

getFlags :: IO Flags
getFlags =
  getProgName >>= cmdArgs . allModes

-- | Command line flags
data Flags = Remotes2LocalFlags
             {
               ignore_tags :: Bool
             , ignore_branches :: Bool
             , stay_orig_tags :: Bool
             , dry_run :: Bool
             , force :: Bool
             }
           | FixFixWordCommitFlags
             { delete :: Bool
             , include_branch :: [String]
             , exclude_branch :: [String]
             , dry_run :: Bool
             }
  deriving (Show, Data, Typeable)

-- | Common command line flags definition
allModes :: String -> Flags
allModes programName =
  modes [remotes2LocalFlags, fixFixWordCommitFlags] &=
  program programName &=
  summary ("Git helper. Misc utilities related to 'git'. Version " ++ programVersion) &=
  help ("Available commands: " ++ (intercalate ", " [remotes2LocalModeName, fixFixWordCommitModeName]))
  where
    programVersion = showVersion version ++ " (haskell)"


forceMsg x = x &= help "Without this option user will always be prompted before changes will be applied. With this option changes will be applied without user intervension"

dryRunMsg x = x &= help "Do not modify anything. Only print what will be done" &= explicit &= name "dry-run"


-- | Commmand line flags definition for 'Remotes to Local' mode
remotes2LocalFlags :: Flags
remotes2LocalFlags =
  Remotes2LocalFlags
  { ignore_tags =
      def &=
      help "Do not process tags"
  , ignore_branches =
      def &=
      help "Do not process branches"
  , stay_orig_tags =
      def &=
      help "Do not delete original tags from commits related to remote branches"
  , dry_run =
      dryRunMsg def
  , force =
      forceMsg def
  } &=
  name remotes2LocalModeName &=
  help "Make local copy for every remote branch and tag" &=
  details [ "Copy remote repos tags and branches to local repo and prefixing every created name with corresponding remote repo name."
          , "Let's say you have remote repo with the name `some_repo`. Then if tag with name `valuable_commit` exists in you local repo" ++
            " and this tag could apear from `some_repo` then tag with name `some_repo/valuable_commit` will be created and original tag `valuable_commit` will be deleted."
          , "For every remote branch a local branch will be created: the name of new branch will be prefixed with remote repo name the same way as for tags."
          ]

remotes2LocalModeName = "remotes2local"

-- | Commmand line flags definition for 'Fix fix word commit' mode
fixFixWordCommitFlags :: Flags
fixFixWordCommitFlags =
  FixFixWordCommitFlags
  { delete =
      def &=
      help "Delete remote branches instead of reverting"
  , include_branch =
      def &=
      help "Force this branch to be corrected. This way you can correct branches that was affected by 'fix word commit', but not selected by default algorythm. Option can be specified multiply times for different branches" &=
      typ "BRANCH"
  , exclude_branch =
      def &=
      help "Prevent this branch to be corrected. Option can be specified multiply times for different branches" &=
      typ "BRANCH"
  , dry_run =
      dryRunMsg def
  } &=
  name fixFixWordCommitModeName &=
  help "Fix repo after `fix word commit` script used in Peter-Service" &=
  details [ "In Peter-Service to all git repositories the fix-word-commit script was applied for correction of problem with MS Word files with documentation. That script applied to many branches that does not require it."
          , "This utility is designed to revert that unnecessary changes. Before changes will be applied you will be sked about your agreement."
          ]

fixFixWordCommitModeName = "fix-fix-word-commit"
