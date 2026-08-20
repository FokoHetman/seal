{-# LANGUAGE OverloadedStrings #-}
module Globals where
import qualified Data.Text as T
import qualified Data.ByteString as BS

globalName :: String
globalName = "seal"
publicHRP :: T.Text
publicHRP = "seal"
privateHRP :: T.Text
privateHRP = "SECRET-STAMP-"

header :: BS.ByteString
header = "SEAL"

info, nonceinfo :: BS.ByteString
info = "seal/v1/wrappingkey"
nonceinfo = "seal/v1/nonce"

macexpand :: BS.ByteString
macexpand = "seal/v1/mac"

maclength :: Int
maclength = 32

wrap :: BS.ByteString -> BS.ByteString -> BS.ByteString
wrap x s = "-----START " <> x <> "-----\n" <> s <> "\n-----END " <> x <> "-----"
