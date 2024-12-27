module Codegen.Utils where

import qualified Ast.Types as AT
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

-- | Extracts the location from an AST node.
getLoc :: AT.Expr -> AT.SrcLoc
getLoc expr = case expr of
  AT.Lit loc _ -> loc
  AT.Var loc _ _ -> loc
  AT.StructAccess loc _ _ -> loc
  AT.ArrayAccess loc _ _ -> loc
  AT.UnaryOp loc _ _ -> loc
  AT.Call loc _ _ -> loc
  AT.If loc _ _ _ -> loc
  AT.While loc _ _ -> loc
  AT.For loc _ _ _ _ -> loc
  AT.Return loc _ -> loc
  AT.Break loc -> loc
  AT.Continue loc -> loc
  AT.Cast loc _ _ -> loc
  AT.Declaration loc _ _ _ -> loc
  AT.Assignment loc _ _ -> loc
  AT.Op loc _ _ _ -> loc
  AT.Function loc _ _ _ _ -> loc
  AT.Block exprs -> getLoc $ head exprs
