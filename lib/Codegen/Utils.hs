module Codegen.Utils where

import qualified Data.ByteString.Short as BS
import qualified LLVM.AST as AST

-- | Converts a string to a `ByteString`.
stringToByteString :: String -> BS.ShortByteString
stringToByteString = BS.pack . map (fromIntegral . fromEnum)

-- | Converts a `ByteString` to a string.
byteStringToString :: BS.ShortByteString -> String
byteStringToString = map (toEnum . fromIntegral) . BS.unpack

-- | Converts AST.Name to a string.
nameToString :: AST.Name -> String
nameToString (AST.Name n) = byteStringToString n
nameToString (AST.UnName n) = show n
