{-# LANGUAGE TemplateHaskell, OverloadedRecordDot, OverloadedStrings #-}
module Subcommands.Decrypt where
import Crypto.PubKey.Curve25519 qualified as X25519
import Crypto.Cipher.ChaChaPoly1305 qualified as CCP

import Control.Lens (makeLenses)
import Subcommands
import Data.Default
import qualified Crypto.Error as CE
import Data.Bool (bool)
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Text as T
import Globals
import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA
import System.Exit (exitSuccess)
import Control.Monad (when, unless, forM, guard)
import Data.Binary.Get (runGet, getBytes, getWord8, getWord32be, getWord16be, getRemainingLazyByteString, getByteString)
import Debug.Trace (trace, traceShow, traceIO)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import Data.Binary.Put (runPut, putWord16be)
import Data.Maybe (listToMaybe)
import Crypto.Hash (SHA256(SHA256))
import qualified Crypto.KDF.HKDF as HKDF
import qualified Crypto.MAC.HMAC as HMAC

data Decrypt = Decrypt
  { __help    :: Bool
  , _identity :: [X25519.SecretKey]
  , _input    :: Maybe FilePath
  }
makeLenses ''Decrypt

readSecretKey :: String -> X25519.SecretKey
readSecretKey key = case Bech32.decode $ T.pack key of
  Right (hrp, datapart) -> bool
    undefined
    (case Bech32.dataPartToBytes datapart of
      Just x  -> case X25519.secretKey x of
        CE.CryptoFailed e -> error $ show e
        CE.CryptoPassed x' -> x'
      Nothing -> undefined
    )
    (Bech32.humanReadablePartToText hrp == T.toLower privateHRP)
  Left e -> error $ "failed reading public key: " <> show e

instance Default Decrypt where
  def = Decrypt False [] Nothing
instance Subcommand' Decrypt where
  names = ["d", "decrypt"]
  flags =
    [ (["-i", "--identity"], "", FlagBuilder $ MultipleValues "IDENTITY" identity readSecretKey)
    ]
  args =
    [ ("INPUT", "File to decrypt. STDIN if not provided.", ArgBuilder $ ArgBuilder' input Just)
    ]
  run d = do
    when d.__help $ do
      putStrLn $ help @Decrypt
      exitSuccess
    Right content <- B64.decode . BC.strip . BS.concat . filter (not . BC.isPrefixOf "--") . BC.lines <$> case d._input of
      Just x -> BS.readFile x
      Nothing-> BS.getContents

    -- STAMP: figure out why it's mismatching against encrypt.
    let (public_key', stanzas, ciphered, macBytes) = runGet (do
          b <- getByteString 4
          guard $ b == header
          v <- getWord8
          unless (v == 0x01) $ error $ "incompatible version: " <> show v
          public_len <- getWord16be
          public_key <- getByteString $ fromIntegral public_len
          stanzas_count <- getWord16be
          stanzas <- forM [1..stanzas_count] . const $ do
            v   <- getWord8
            guard $ v == 0x01
            len <- getWord16be
            getByteString $ fromIntegral len
          body <- BS.toStrict <$> getRemainingLazyByteString
          let (ciphered, macBytes) = BS.splitAt (BS.length body - maclength) body
          pure (public_key, stanzas, ciphered, macBytes)
          ) $ BS.fromStrict content
    let (ciphertext, tag) = BS.splitAt (BS.length ciphered - 16) ciphered
        public_key = case X25519.publicKey public_key' of
            CE.CryptoFailed e -> error $ show e
            CE.CryptoPassed x -> x
    let filekey = case getFileKey public_key d._identity stanzas of
            Just x -> x
            Nothing -> error "no private key found"

        prk = HKDF.extract @SHA256 ("" :: BS.ByteString) filekey
        macKey = HKDF.expand prk ("seal/v1/mac" :: BS.ByteString) 32 :: BS.ByteString

        expectedMac = HMAC.hmac @BS.ByteString @BS.ByteString @SHA256 macKey $ BS.dropEnd 32 content
        expectedMacBytes = BA.convert expectedMac :: BS.ByteString

        state = case CCP.nonce12 (BS.replicate 12 0) >>= CCP.initialize filekey of
          CE.CryptoFailed err -> error $ show err
          CE.CryptoPassed x -> x
        state' = CCP.finalizeAAD $ CCP.appendAAD header state
        (decrypted, state'') = CCP.decrypt ciphertext state'
        realTag = BA.convert $ CCP.finalize state'' :: BS.ByteString
    when (tag /= realTag) $ error "unexpected tag"
    when (macBytes /= expectedMacBytes) $ error "unexpected mac"
    BC.putStr decrypted
  help = subcommandHelp @Decrypt


getFileKey :: X25519.PublicKey -> [X25519.SecretKey] -> [BS.ByteString] -> Maybe BS.ByteString
getFileKey public_key keys stanzas = listToMaybe [x | key <- keys, stanza <- stanzas, Just x <- [tryUnwrap key stanza]]
  where
    tryUnwrap :: X25519.SecretKey -> BS.ByteString -> Maybe BS.ByteString
    tryUnwrap key stanza = do
      guard $ BS.length stanza == 48
      let shared = X25519.dh public_key key
          prk = HKDF.extract @SHA256 ("" :: BS.ByteString) shared
          wrappingKey = HKDF.expand prk ("seal/v1/wrappingkey" :: BS.ByteString) 32 :: BS.ByteString
          nonce = HKDF.expand prk ("seal/v1/nonce" :: BS.ByteString) 12 :: BS.ByteString
      
      let (ciphered, tag) = BS.splitAt (BS.length stanza - 16) stanza
      state <- case CCP.nonce12 nonce >>= CCP.initialize wrappingKey of
        CE.CryptoFailed _ -> Nothing
        CE.CryptoPassed x -> Just x
      let state' = CCP.finalizeAAD $ CCP.appendAAD header state
          (ciphertext, state'') = CCP.decrypt ciphered state'
          tag' = CCP.finalize state''
      bool Nothing (Just ciphertext) $ tag == BA.convert tag'


instance CliParse Decrypt where
  cliParser = subcommandParser
