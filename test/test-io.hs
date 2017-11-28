import           Control.Monad (unless)

import           System.Exit
import           System.IO

import qualified Test.IO.Huck.Lexer

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [
      Test.IO.Huck.Lexer.tests
    ]

  --
  -- Normally we would exit with failure when tests fail using something like:
  --
  unless (and results) $
    System.Exit.exitFailure
  --
  -- But this project is designed to actually show test errors as an example so
  -- we don't want it to break CI.
  --

  pure ()
