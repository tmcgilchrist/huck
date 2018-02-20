import           Control.Monad (unless)

import           System.Exit (exitFailure)
import           System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import qualified Test.Huck.Lenses
import qualified Test.Huck.Lexer
import qualified Test.Huck.Parser

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [
      Test.Huck.Lexer.tests
    , Test.Huck.Parser.tests
    , Test.Huck.Lenses.tests
    ]

  unless (and results) System.Exit.exitFailure
  pure ()
