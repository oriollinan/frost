module Main where

import qualified Ast.Types as T
import qualified Codegen.Codegen as C
import qualified Data.Text.Lazy as TL
import qualified LLVM.Pretty as P

main :: IO ()
main = do
  let ast =
        T.AST
          [ T.Define
              "$$generated"
              ( T.If
                  (T.Lit (T.LInt 1))
                  ( T.Call
                      ( T.Lambda ["x"] (T.Op T.Add (T.Var "x") (T.Lit (T.LInt 1)))
                      )
                      [T.Lit (T.LInt 2)]
                  )
                  ( T.Call
                      ( T.Lambda ["y"] (T.Op T.Sub (T.Var "y") (T.Lit (T.LInt 1)))
                      )
                      [T.Lit (T.LInt 2)]
                  )
              )
          ]

  let output = TL.unpack $ P.ppllvm $ C.codegen ast
  writeFile "demo/generated.ll" output
