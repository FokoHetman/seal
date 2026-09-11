{-# LANGUAGE TemplateHaskell, OverloadedRecordDot #-}
module Subcommands.Agent.Add where
import qualified Data.ByteString as BS
import Data.Default
import Subcommands
import Control.Lens (makeLenses)
import System.Exit (exitSuccess)
import Control.Monad (when)

data Add = Add
  { __help  :: Bool
  , _keys   :: [BS.ByteString]--[SealKey]
  , _files  :: [FilePath]
  }
makeLenses ''Add
parseAddKey = undefined

instance Default Add where
  def = Add False [] []

instance CliParse Add where
  cliParser = subcommandParser

instance Subcommand' Add where
  names = ["a", "add"]
  flags = 
    [ (["-k", "--key"], "keys to add to the agent", FlagBuilder $ MultipleValues "KEY" keys parseAddKey)
    , (["-f", "--file"], "files with key/s to add to the agent", FlagBuilder $ MultipleValues "FILE" files id)
    ]
  args = []
  run add = do
    when add.__help $ do
      putStrLn $ help @Add
      exitSuccess
    sock <- connectAgent
    keys <- mapM (fmap (fmap parseAddKey . BS.split 0x20) . BS.readFile) add._files
    sendAgent sock $ "ADD\n" <> concatMap (<>"\n") (keys <> add._keys)
    undefined
  help = subcommandHelp @Add
