{-# LANGUAGE TemplateHaskell, OverloadedStrings, OverloadedRecordDot #-}
module Subcommands.Keygen where
import Subcommands
import Data.Default
import Data.ByteArray
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Crypto.PubKey.Curve25519 qualified as Curve
import Crypto.Cipher.ChaChaPoly1305 qualified as CCP
import Codec.Binary.Bech32 qualified as Bech32
import qualified Chronos
import qualified Network.HostName as HN
import Data.Time (getCurrentTimeZone, TimeZone (timeZoneMinutes))
import Control.Lens (makeLenses)
import System.Exit (exitSuccess)
import Control.Monad (when)
import Globals (privateHRP, publicHRP)


data Keygen = Keygen
  { __help :: Bool
  }
makeLenses ''Keygen

instance Default Keygen where
  def = Keygen False

instance CliParse Keygen where
  cliParser = subcommandParser

instance Subcommand' Keygen where
  names = ["k", "keygen"]
  flags = [(["-h", "--help"], "Display this message.", FlagBuilder $ ExistentialValue _help)]
  args = []

  help = subcommandHelp @Keygen
  run Keygen{__help} = do
    when __help $ do
      putStrLn $ help @Keygen
      exitSuccess
    secretkey <- Curve.generateSecretKey
    let publickey = Curve.toPublic secretkey
    let publickey' = case Bech32.humanReadablePartFromText publicHRP of
          Left _ -> undefined
          Right hrp -> Bech32.encodeLenient hrp datapart
            where
              datapart = Bech32.dataPartFromBytes $ convert publickey
    let privatekey' = case Bech32.humanReadablePartFromText privateHRP of
          Left _ -> undefined
          Right hrp -> T.toUpper $ Bech32.encodeLenient hrp datapart
            where
              datapart = Bech32.dataPartFromBytes $ convert secretkey

    tz <- getCurrentTimeZone
    let offset = Chronos.Offset $ tz.timeZoneMinutes
        fmt = Chronos.DatetimeFormat
          { datetimeFormatDateSeparator = Just '-'
          , datetimeFormatSeparator     = Just 'T'
          , datetimeFormatTimeSeparator = Just ':'
          }
    now <- Chronos.timeToOffsetDatetime offset <$> Chronos.now
    hostname <- T.pack <$> HN.getHostName
    T.putStrLn $ T.concat
      [ "# generated at: ", Chronos.encode_YmdHMSz (Chronos.OffsetFormatColonOn) (Chronos.SubsecondPrecisionFixed 0) (fmt) now, "\n"
      , "# on: ", hostname, "\n"
      , "# public key: ", publickey', "\n"
      , privatekey'
      ]
