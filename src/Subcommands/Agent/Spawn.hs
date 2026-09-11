{-# LANGUAGE TemplateHaskell, OverloadedRecordDot #-}
module Subcommands.Agent.Spawn where
import Subcommands
import Data.Default
import Control.Lens (makeLenses)
import Control.Monad (when)
import System.Exit (exitSuccess)

data Spawn = Spawn
  { __help :: Bool
  }

makeLenses ''Spawn

instance Default Spawn where
  def = Spawn False

instance CliParse Spawn where
  cliParser = subcommandParser

instance Subcommand' Spawn where
  names = ["s", "spawn"]
  flags = []
  args = []
  run spawn = do
    when spawn.__help $ do
      putStrLn $ help @Spawn
      exitSuccess
    undefined
  help = subcommandHelp @Spawn
