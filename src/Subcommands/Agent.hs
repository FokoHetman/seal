{-# LANGUAGE TemplateHaskell, OverloadedRecordDot #-}
module Subcommands.Agent where
import Subcommands
import Control.Lens (makeLenses, (?~))
import Data.Default
import Data.Functor
import Control.Applicative
import Data.List (intercalate)
import Globals (globalName)
import Data.Bool (bool)

import Subcommands.Agent.Add (Add)

data Agent = Agent
  { _output     :: Maybe FilePath
  , __help      :: Bool
  , _subcommand :: Maybe Subcommand
  }
makeLenses ''Agent

instance Default Agent where
  def = Agent
    { _output = Nothing
    , _subcommand = Nothing
    , __help = False
    }


cmds = [SubcommandW @Add]

instance CliParse Subcommand where
  cliParser = foldl (<|>) (CliParser . const $ Left Undefined) $ fmap cliParserW cmds


instance CliParse Agent where
  cliParser = flip (foldr (.) id) def <$> many (f <|> foldr ((<|>) . flagToParser) empty flags)

f :: CliParser (Agent -> Agent)
f = cliParser <&> (subcommand ?~)

instance Subcommand' Agent where
  names = []
  args = []
  flags =
      [ (["-o", "--output"], "define output of the command. STDIN if not set.", FlagBuilder $ SingleValue "OUTPUT" output id)
      , (["-h", "--help"], "display this message.", FlagBuilder $ ExistentialValue _help)
      ]
  run agent = case agent._subcommand of
    Just (Subcommand x) -> bool
      (run x)
      (putStrLn $ help @Agent)
      agent.__help
    Nothing -> putStrLn $ help @Agent

  help = concat
      [ globalName, " ", intercalate " | " $ fmap showFlagSmall $ flags @Agent, " [subcommand] ..args\n"
      , "global flags:\n\n", displayFlags @Agent, "\n\n"
      , "subcommands:\n\n"
      , intercalate "\n\n" $ fmap (\(SubcommandW @a) -> help @a) cmds
      ]
