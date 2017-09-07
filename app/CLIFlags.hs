{-  -*- coding:utf-8 -*-  -}
{-# OPTIONS_GHC -fno-cse #-}
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
               tag :: [String]
             , no_tag :: [String]
             , branch :: [String]
             , no_branch :: [String]
             , repo :: [String]
             , no_repo :: [String]
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
  verbosity &=
  program programName &=
  summary ("Git helper. Misc utilities related to 'git'. Version " ++ programVersion) &=

  help ("Commands: \n" ++ (intercalate "\n" . map ("  "++)) [remotes2LocalModeName, fixFixWordCommitModeName])
  where
    programVersion = showVersion version ++ " (haskell)"


forceMsg x = x &= help "Without this option user will always be prompted before changes will be applied. With this option changes will be applied without user intervension"

dryRunMsg x = x &= help "Do not modify anything. Only print what will be done" &= explicit &= name "dry-run"

regexTyp = "REGEX"


-- | Commmand line flags definition for 'Remotes to Local' mode
remotes2LocalFlags :: Flags
remotes2LocalFlags =
  Remotes2LocalFlags
  {
    repo =
      def &=
      typ regexTyp &=
      help ("Process only remote repos that names (as `git remote` command prints) match the "++regexTyp++". If option is specified multiply times then all remote repos that names match any of specified patterns will be selected")
  , no_repo =
      def &=
      typ regexTyp &=
      help ("Ignore remote repos that names (as `git remote` command prints) match the "++regexTyp++". If option is specified multiply times then all remote repos that names match any of specified patterns will be ignored. This option take precedence over `--repo`: if some tag match both options then it will be ignored")
  , tag =
      def &=
      typ regexTyp &=
      help ("Process only tags that names match the "++regexTyp++". If option is specified multiply times then all tags that match any of specified patterns will be selected")
  , no_tag =
      def &=
      opt ".*" &=
      typ regexTyp &=
      help ("Ignore tags that names match the "++regexTyp++". If option is specified multiply times then all tags that match any of specified patterns will be ignored. If given without pattern then all tags will be ignored. This option take precedence over `--tag`: if some tag match both options then it will be ignored")
  , branch =
      def &=
      typ regexTyp &=
      help ("Process only branches that names match the "++regexTyp++". If option is specified multiply times then all branches that match any of specified patterns will be selected")
  , no_branch =
      def &=
      opt ".*" &=
      typ regexTyp &=
      help ("Ignore branches that names match the "++regexTyp++". If option is specified multiply times then all branches that match any of specified patterns will be ignored. If given without pattern then all branches will be ignored. This option take precedence over `--branch`: if some branch match both options then it will be ignored")
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
