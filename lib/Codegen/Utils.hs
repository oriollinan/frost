module Codegen.Utils where

import qualified Data.ByteString.Short as BS

-- | Converts a string to a `ByteString`.
stringToByteString :: String -> BS.ShortByteString
stringToByteString = BS.pack . map (fromIntegral . fromEnum)

-- | Converts a `ByteString` to a string.
byteStringToString :: BS.ShortByteString -> String
byteStringToString = map (toEnum . fromIntegral) . BS.unpack
