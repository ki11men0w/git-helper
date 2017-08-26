{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import CLIFlags
import qualified PrepareRemotesMode

main :: IO ()
main = do
  flags <- getFlags
  case flags of
    PrepareRemotesFlags{} -> PrepareRemotesMode.run flags
  return ()
