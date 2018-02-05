import Data.Char
import System.Exit
import Control.Monad
import TokenTypes

data NfaState = NfaState { transition :: (Char -> NfaState)
                         , accepting :: Bool
                         }

data Nfa = Nfa { allStates :: [NfaState]
               , currentState :: [NfaState]
               }

main :: IO ()
main = do
  contents <- getContents
  let nonAscii = any (not . isAscii) contents
  when (nonAscii) (exitError "Invalid non-ascii characters")

regexToNfa :: Recognizer -> Nfa
regexToNfa r = Nfa [] []

exitError e = do
  putStrLn e
  exitWith $ ExitFailure 42
