{- -*- coding: utf-8 -*- -}
module ProcessLib
  ( runCom
  ) where

import System.Process
import System.Exit (ExitCode(..))
import Data.Text

gitError code = error $ "git exited with error code " ++ show code

-- | Run `commandLine` in a shell. If command exites with non zero exit code
--   then error will be called.
runCom :: Bool -> Text -> IO ()
runCom dryRun commandLine =
  let commandLine' = unpack commandLine
  in
    if dryRun
      then putStrLn commandLine'
      else withCreateProcess (shell commandLine') $ \_ _ _ h -> do
        exitCode <- waitForProcess h
        case exitCode of
          ExitFailure code -> gitError code
          ExitSuccess -> return ()
