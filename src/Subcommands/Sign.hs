{-# LANGUAGE TemplateHaskell, OverloadedRecordDot, OverloadedStrings #-}
module Subcommands.Sign where
import Control.Lens (makeLenses)
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Crypto.Error qualified as CE
import Data.Default
import Subcommands
import Globals
import Control.Monad (when)
import System.Exit (exitSuccess)
import Data.Bool (bool)
import qualified Codec.Binary.Bech32 as Bech32
import Data.Text qualified as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteArray as BA

readSecretKey :: String -> Ed25519.SecretKey
readSecretKey key = case Bech32.decode $ T.pack key of
  Right (hrp, datapart) -> bool
    undefined
    (case Bech32.dataPartToBytes datapart of
      Just x  -> case Ed25519.secretKey x of
        CE.CryptoFailed e -> error $ show e
        CE.CryptoPassed x' -> x'
      Nothing -> undefined
    )
    (Bech32.humanReadablePartToText hrp == T.toLower privateHRP)
  Left e -> error $ "failed reading public key: " <> show e


data Sign = Sign
  { __help      :: Bool
  , _input      :: Maybe FilePath
  , _signatory  :: [Ed25519.SecretKey]
  }
makeLenses ''Sign

instance Default Sign where
  def = Sign False Nothing []

instance Subcommand' Sign where
  names = ["s", "sign"]
  flags =
    [ (["-h", "--help"], "Display this message.", FlagBuilder $ ExistentialValue _help)
    , (["-s", "--signatory"], "Define (list of) signatories of the message.", FlagBuilder $ MultipleValues "SIGNATORY" signatory readSecretKey)
    ]
  args =
    [ ("INPUT", "File to sign. STDIN if not provided.", ArgBuilder $ ArgBuilder' input Just)
    ]
  run s = do
    when s.__help $ do
      putStrLn $ help @Sign
      exitSuccess
    content <- case s._input of
      Just x -> BS.readFile x
      Nothing-> BS.getContents
    BS.putStr $ BS.intercalate "\n" $ fmap (B64.encode . BA.convert) [Ed25519.sign signatory' (Ed25519.toPublic signatory') content | signatory' <- s._signatory]
  help = subcommandHelp @Sign

instance CliParse Sign where
  cliParser = subcommandParser
