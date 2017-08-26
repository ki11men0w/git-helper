{-  -*- coding:utf-8 -*-  -}
module PrepareRemotesMode (run) where

import Data.Monoid ((<>))
import CLIFlags (Flags(PrepareRemotesFlags))

checkFlags :: Flags -> IO ()
checkFlags PrepareRemotesFlags{} =
  return ()
checkFlags alien =
  error $ "Can not process " <> (show alien)


run :: Flags -> IO ()
run flags = do
  checkFlags flags
  print flags
