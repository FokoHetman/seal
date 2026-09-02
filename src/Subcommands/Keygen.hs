{-# LANGUAGE TemplateHaskell, OverloadedStrings, OverloadedRecordDot #-}
module Subcommands.Keygen where
import Subcommands
import Data.Default
import Data.ByteArray
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Crypto.PubKey.Curve25519 qualified as X25519
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Codec.Binary.Bech32 qualified as Bech32
import qualified Chronos
import qualified Network.HostName as HN
import Data.Time (getCurrentTimeZone, TimeZone (timeZoneMinutes))
import Control.Lens (makeLenses)
import System.Exit (exitSuccess)
import Control.Monad (when)
import Globals (privateHRP, publicHRP)
import qualified Data.ByteString as BS

data KeyType = Encryption | Signing

parseKeyType :: String -> Maybe KeyType
parseKeyType "encryption" = Just Encryption
parseKeyType "curve25519" = Just Encryption
parseKeyType "signing" = Just Signing
parseKeyType "ed25519" = Just Signing
parseKeyType _ = Nothing

mkKey :: KeyType -> IO (BS.ByteString, BS.ByteString)
mkKey Encryption = do
  secretkey <- X25519.generateSecretKey
  pure (convert secretkey, convert $ X25519.toPublic secretkey)
mkKey Signing = do
  secretkey <- Ed25519.generateSecretKey
  pure (convert secretkey, convert $ Ed25519.toPublic secretkey)

data Keygen = Keygen
  { __help    :: Bool
  , _keytype  :: Maybe KeyType
  }
makeLenses ''Keygen

instance Default Keygen where
  def = Keygen False Nothing

instance CliParse Keygen where
  cliParser = subcommandParser

instance Subcommand' Keygen where
  names = ["k", "keygen"]
  flags = [(["-h", "--help"], "Display this message.", FlagBuilder $ ExistentialValue _help)]
  args = [("key","type of key to generate: {encryption/curve25519, signing/ed25519}", ArgBuilder $ ArgBuilder' keytype parseKeyType)]

  help = subcommandHelp @Keygen
  run k = do
    when k.__help $ do
      putStrLn $ help @Keygen
      exitSuccess
    keytype' <- case k._keytype of
      Just x -> pure x
      Nothing-> error "no correct keytype given"
    (secretkey, publickey) <- mkKey keytype'
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
