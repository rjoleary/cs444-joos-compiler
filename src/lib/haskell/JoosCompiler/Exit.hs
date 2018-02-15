module JoosCompiler.Exit where

import           System.Exit
import           System.IO

exitError e = do
  hPutStrLn stderr e
  exitWith $ ExitFailure 42
