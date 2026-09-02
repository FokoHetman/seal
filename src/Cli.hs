{-# LANGUAGE TemplateHaskell, OverloadedRecordDot #-}
module Cli where

import Control.Applicative
import Control.Lens
import Data.Char (isNumber)
import Data.Bool (bool)
import Data.Default

import Subcommands
import Subcommands.Decrypt (Decrypt)
import Subcommands.Encrypt (Encrypt)
import Subcommands.Keygen (Keygen)
import Data.List (intercalate)
import Globals (globalName)
import Subcommands.Verify (Verify(Verify))
import Subcommands.Sign (Sign(Sign))


segments :: String -> [String]
segments [] = []
segments (x:xs) = bool
  (['-', x]:segments xs)
  (case span isNumber xs of
    (xs',y) -> (x:xs'):segments y
  )
  (isNumber x)

splitFlags :: String -> [String]
splitFlags [] = []
splitFlags ('-':'-':rest) = ['-':'-':rest]
splitFlags ('-':flags) = segments flags
splitFlags a = [a]

cmds :: [SubcommandW]
cmds  = [ SubcommandW @Encrypt
        , SubcommandW @Decrypt
        , SubcommandW @Keygen
        , SubcommandW @Sign
        , SubcommandW @Verify
        ]

instance CliParse Subcommand where
  cliParser = foldl (<|>) (CliParser . const $ Left Undefined) $ fmap cliParserW cmds

data Cli = Cli
  { _output     :: Maybe FilePath
  , __help      :: Bool
  , _subcommand :: Maybe Subcommand
  }
makeLenses ''Cli

instance Default Cli where
  def = Cli
    { _output = Nothing
    , _subcommand = Nothing
    , __help = False
    }

instance CliParse Cli where
  cliParser = (($ def) <$> foldr (.) id <$> many (f <|> (foldr (<|>) empty $ fmap flagToParser flags)))

f :: CliParser (Cli -> Cli)
f = cliParser <&> (subcommand .~) . Just

instance Subcommand' Cli where
  names = []
  args = []
  flags =
      [ (["-o", "--output"], "define output of the command. STDIN if not set.", FlagBuilder $ SingleValue "OUTPUT" output id)
      , (["-h", "--help"], "display this message.", FlagBuilder $ ExistentialValue _help)
      ]
  run cli = case cli._subcommand of
    Just (Subcommand x) -> bool
      (run x)
      (putStrLn $ help @Cli)
      cli.__help
    Nothing -> putStrLn $ help @Cli

  help = concat
      [ globalName, " ", intercalate " | " $ fmap showFlagSmall $ flags @Cli, " [subcommand] ..args\n"
      , "global flags:\n\n", displayFlags @Cli, "\n\n"
      , "subcommands:\n\n"
      , intercalate "\n\n" $ fmap (\(SubcommandW @a) -> help @a) cmds
      ]
