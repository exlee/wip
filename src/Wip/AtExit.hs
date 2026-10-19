-- | Exit hooks, the counterpart of OCaml's @at_exit@: the executables run
-- 'runExitHooks' when @main@ finishes or throws.
module Wip.AtExit (atExit, runExitHooks) where

import Control.Exception (SomeException, try)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

hooks :: IORef [IO ()]
hooks = unsafePerformIO (newIORef [])
{-# NOINLINE hooks #-}

-- | Registers an action; hooks run newest first.
atExit :: IO () -> IO ()
atExit h = atomicModifyIORef' hooks (\hs -> (h : hs, ()))

-- | Runs and forgets every hook; a failing hook does not stop the others.
runExitHooks :: IO ()
runExitHooks = do
  hs <- atomicModifyIORef' hooks (\hs -> ([], hs))
  mapM_ (\h -> try h :: IO (Either SomeException ())) hs
