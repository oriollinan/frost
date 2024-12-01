module Main where

import qualified Ast.Types as T
import qualified Codegen.Codegen as C
import qualified Data.Text.Lazy as TL
import qualified LLVM.Pretty as P

main :: IO ()
main = do
  let ast =
        T.Expr
          ( T.If
              (T.Lit (T.LInt 1))
              (T.Op T.Add (T.Lit (T.LInt 2)) (T.Lit (T.LInt 3)))
              (T.Op T.Multiply (T.Lit (T.LInt 4)) (T.Lit (T.LInt 5)))
          )
  let output = TL.unpack $ P.ppllvm $ C.codegen ast
  writeFile "demo/generated.ll" output
