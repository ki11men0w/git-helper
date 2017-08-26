{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE DeriveDataTypeable #-}
module CLIFlags (Flags(..), getFlags) where

import Paths_git_helper (version)
import Data.Version (showVersion)
import System.Environment (getProgName)
import System.Console.CmdArgs

-- | Command line flags
data Flags = PrepareRemotesFlags {
    ignore_tags :: Bool
  , ignore_branches :: Bool
  , stay_orig_tags :: Bool
  }
  deriving (Show, Data, Typeable)

-- | Common command line flags definition
allModes :: String -> Flags
allModes programName =
  modes [prepareRemotesFlags] &=
  program programName &=
  summary ("Git helper. Misc utilities related to 'git'. Version " ++ programVersion)
  where
    programVersion = showVersion version ++ " (haskell)"

-- | Commmand line flags definition for 'Prepare remotes' mode
prepareRemotesFlags :: Flags
prepareRemotesFlags =
  PrepareRemotesFlags
  { ignore_tags =
      def &=
      help "Do not process tags"
  , ignore_branches =
      def &=
      help "Do not process branches"
  , stay_orig_tags =
      def &=
      help "Do not delete original tags from commits on remote branches"
  } &=
  name "prepare-remotes" &=
  details ["Prepare remotes mode details here"]


getFlags :: IO Flags
getFlags =
  getProgName >>= cmdArgs . allModes
