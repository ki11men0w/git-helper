{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import CLIFlags
import qualified Remotes2LocalMode
import qualified FixFixWordCommitMode

main :: IO ()
main = do
  flags <- getFlags
  case flags of
    Remotes2LocalFlags{} -> Remotes2LocalMode.run flags
    FixFixWordCommitFlags{} -> FixFixWordCommitMode.run flags
