{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE DeriveDataTypeable #-}
module CLIFlags (Flags(..), getFlags) where

import Paths_git_helper (version)
import Data.Version (showVersion)
import System.Environment (getProgName)
import System.Console.CmdArgs

-- | Command line flags
data Flags = Remotes2LocalFlags {
    ignore_tags :: Bool
  , ignore_branches :: Bool
  , stay_orig_tags :: Bool
  , dry_run :: Bool
  , force :: Bool
  }
  deriving (Show, Data, Typeable)

-- | Common command line flags definition
allModes :: String -> Flags
allModes programName =
  modes [prepareRemotes2LocalFlags] &=
  program programName &=
  summary ("Git helper. Misc utilities related to 'git'. Version " ++ programVersion)
  where
    programVersion = showVersion version ++ " (haskell)"


forceMsg x = x &= help "Without this option user will always be prompted before changes will be applied. With this option changes will be applied without user intervension"

-- | Commmand line flags definition for 'Remotes to Local' mode
prepareRemotes2LocalFlags :: Flags
prepareRemotes2LocalFlags =
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
      def &=
      help "Do not modify anything. Only print what will be done"
  , force = forceMsg def
  } &=
  name "remotes2local" &=
  details [ "Copy remote repos tags and branches to local repo and prefixing every created name with corresponding remote repo name."
          , "Let's say you have remote repo with the name `some_repo`. Then if tag with name `valuable_commit` exists in you local repo" ++
            " and this tag could apear from `some_repo` then tag with name `some_repo/valuable_commit` will be created and original tag `valuable_commit` will be deleted."
          , "For every remote branch a local branch will be created: the name of new branch will be prefixed with remote repo name the same way as for tags."
          ]


getFlags :: IO Flags
getFlags =
  getProgName >>= cmdArgs . allModes
