import           Control.Monad (unless)
import           System.Exit
import           System.IO

import qualified Test.Huck.Lexer

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [
      Test.Huck.Lexer.tests
    ]

  unless (and results) $
    System.Exit.exitFailure
  pure ()
