{-# LANGUAGE TemplateHaskell, OverloadedRecordDot, OverloadedStrings #-}
module Subcommands.Verify where
import Control.Lens (makeLenses)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Default
import Subcommands
import Globals
import Control.Monad (when)
import System.Exit (exitSuccess)
import qualified Crypto.Error as CE
import qualified Codec.Binary.Bech32 as Bech32
import Data.Bool (bool)
import Data.Text qualified as T
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA

data Verify = Verify
  { __help      :: Bool
  , _input      :: Maybe FilePath
  , _signature  :: [Ed25519.Signature]
  , _signatory  :: [Ed25519.PublicKey]
  }
makeLenses ''Verify

readSignature :: String -> Ed25519.Signature
readSignature sig = case B64.decode $ T.encodeUtf8 $ T.pack sig of
  Left err -> error err
  Right sig' -> case Ed25519.signature sig' of
    CE.CryptoFailed err -> error $ show err
    CE.CryptoPassed x -> x

readSignatory :: String -> Ed25519.PublicKey
readSignatory key = case Bech32.decode $ T.pack key of
  Right (hrp, datapart) -> bool
    undefined
    (case Bech32.dataPartToBytes datapart of
      Just x  -> case Ed25519.publicKey x of
        CE.CryptoFailed e -> error $ show e
        CE.CryptoPassed x' -> x'
      Nothing -> undefined
    )
    (Bech32.humanReadablePartToText hrp == publicHRP)
  Left e -> error $ "failed reading public key: " <> show e

formatSignatory :: Ed25519.PublicKey -> BS.ByteString
formatSignatory key = case Bech32.humanReadablePartFromText publicHRP of
  Left err -> error $ show err
  Right hrp-> T.encodeUtf8 $ Bech32.encodeLenient hrp datapart
    where
      datapart = Bech32.dataPartFromBytes $ BA.convert key

instance Default Verify where
  def = Verify False Nothing [] []

instance Subcommand' Verify where
  names = ["v", "verify"]
  flags =
    [ (["-h", "--help"], "Display this message.", FlagBuilder $ ExistentialValue _help)
    , (["-S", "--signature"], "Define (list of) signatures to verify.", FlagBuilder $ MultipleValues "SIGNATURE" signature readSignature)
    , (["-s", "--signatory"], "Define (list of) signatories to verify.", FlagBuilder $ MultipleValues "SIGNATORY" signatory readSignatory)
    ]
  args =
    [ ("INPUT", "Signed message to verify. STDIN if not provided.", ArgBuilder $ ArgBuilder' input Just)
    ]
  run v = do
    when v.__help $ do
      putStrLn $ help @Verify
      exitSuccess
    content <- case v._input of
      Just x -> BS.readFile x
      Nothing-> BS.getContents
    BS.putStr $ BS.concat
      $ [formatSignatory signatory' <> ", " <> B64.encode (BA.convert signature')
        <> ": " <> bool "INVALID" "VALID" (Ed25519.verify signatory' content signature')
        | signatory' <- v._signatory, signature' <- v._signature]
  help = subcommandHelp @Verify

instance CliParse Verify where
  cliParser = subcommandParser
