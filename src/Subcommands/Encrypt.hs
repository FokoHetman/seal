{-# LANGUAGE OverloadedRecordDot, OverloadedStrings, TemplateHaskell #-}
module Subcommands.Encrypt where
import Subcommands
import Control.Lens (makeLenses)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Default
import Data.ByteArray qualified as BA
import System.Exit (exitSuccess)
import Control.Monad (when)

import Crypto.MAC.HMAC qualified as HMAC
import Crypto.KDF.HKDF qualified as HKDF
import Crypto.PubKey.Curve25519 qualified as X25519
import Crypto.Cipher.ChaChaPoly1305 qualified as CCP
import System.Random (uniformByteString, getStdGen)
import Crypto.Error qualified as CE
import qualified Data.Text as T
import Globals
import Crypto.Hash (SHA256)
import Data.Binary.Put (runPut, putWord16be)
import qualified Data.Text.Encoding as T
import Data.Bool (bool)
import qualified Codec.Binary.Bech32 as Bech32

readPublicKey :: String -> X25519.PublicKey
readPublicKey key = case Bech32.decode $ T.pack key of
  Right (hrp, datapart) -> bool
    undefined
    (case Bech32.dataPartToBytes datapart of
      Just x  -> case X25519.publicKey x of
        CE.CryptoFailed e -> error $ show e
        CE.CryptoPassed x' -> x'
      Nothing -> undefined
    )
    (Bech32.humanReadablePartToText hrp == publicHRP)
  Left e -> error $ "failed reading public key: " <> show e

data Encrypt = Encrypt
  { __help      :: Bool
  , _age        :: Bool
  , _input      :: Maybe FilePath
  , _recipents  :: [X25519.PublicKey]
  }
makeLenses ''Encrypt

instance Default Encrypt where
  def = Encrypt False False Nothing []

instance Subcommand' Encrypt where
  names = ["e", "encrypt"]
  flags =
    [ (["-h", "--help"], "Display this message.", FlagBuilder $ ExistentialValue _help)
    , (["-r", "--recipent", "--seal"], "Define (list of) recipents of encrypted INPUT.", FlagBuilder $ MultipleValues "RECIPENT" recipents readPublicKey)
    , (["--age"], "generate an age-encrypted file instead of a sealed file.", FlagBuilder $ ExistentialValue age)
    ]
  args =
    [ ("INPUT", "File to encrypt. STDIN if not provided.", ArgBuilder $ ArgBuilder' input Just)
    ]
  help = subcommandHelp @Encrypt

  run e = do
    when e.__help $ do
      putStrLn $ help @Encrypt
      exitSuccess
    content <- case e._input of
      Just x -> BS.readFile x
      Nothing-> BS.getContents
    gen <- getStdGen
    let (filekey, _gen') = uniformByteString 32 gen
        nonce :: BS.ByteString = BA.replicate 12 0
        init' = CCP.nonce12 nonce >>= CCP.initialize filekey
        ciphered = case init' of
          CE.CryptoPassed state -> let
            state' = CCP.finalizeAAD $ CCP.appendAAD header state
            (ciphertext, state'') = CCP.encrypt content state'
            tag = CCP.finalize state''
            in BS.append ciphertext $ BA.convert tag
          CE.CryptoFailed err -> error $ show err
    ep_s <- X25519.generateSecretKey
    let ep_p = X25519.toPublic ep_s

    let salt = "" :: BS.ByteString

    let stanzas = fmap (\recipent -> let
          shared :: BS.ByteString = BA.convert $ X25519.dh recipent ep_s
          prk = HKDF.extract @SHA256 salt shared
          wrappingKey :: BS.ByteString = HKDF.expand prk info 32
          nonce':: BS.ByteString= HKDF.expand prk nonceinfo 12
          init2 = CCP.nonce12 nonce' >>= CCP.initialize wrappingKey
          wrappedFileKey = case init2 of
            CE.CryptoPassed state -> let
              state' = CCP.finalizeAAD $ CCP.appendAAD header state
              (ciphertext, state'') = CCP.encrypt filekey state'
              tag = CCP.finalize state''
              in BS.append ciphertext $ BA.convert tag
            CE.CryptoFailed err -> error $ show err
          in wrappedFileKey) e._recipents
    let stanzas' = fmap mkStanza stanzas
          where mkStanza key = BS.cons 0x01 $ BS.append (BS.toStrict . runPut . putWord16be . fromIntegral $ BS.length key) key
                  --where body = BS.concat [BA.convert ep_p, key]
        fileHeader = header <> BS.pack [0x01]
        payload = BS.concat [fileHeader, BS.toStrict . runPut . putWord16be . fromIntegral $ BA.length ep_p, BA.convert ep_p, BS.toStrict . runPut . putWord16be . fromIntegral $ length stanzas, BS.concat stanzas', ciphered]
        prk = HKDF.extract @SHA256 ("" :: BS.ByteString) filekey
        macKey :: BS.ByteString = HKDF.expand prk macexpand maclength
        mac = HMAC.hmac @BS.ByteString @BS.ByteString @SHA256 macKey payload

    let file = B64.encode $ BS.append payload (BA.convert mac)
    BS.putStr $ wrap "SEALED FILE" $ T.encodeUtf8 $ T.intercalate "\n" $ T.chunksOf 64 $ T.decodeUtf8 file
instance CliParse Encrypt where
  cliParser = subcommandParser
