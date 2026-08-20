module Main (main) where
import Cli
import System.Environment (getArgs)
import Subcommands


main :: IO ()
main = do
  args' <- getArgs
  cli :: Cli <- case runCliParser cliParser args' of
    Right (_,x) -> pure x
    Left err -> raiseError "Parser Failure" err
  run cli
