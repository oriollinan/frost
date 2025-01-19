module Codegen.ExprGen.FunctionSpec (spec) where

import qualified Ast.Types as AT
import qualified Codegen.Codegen as CC
import qualified Codegen.Errors as CE
import qualified Codegen.Utils as U
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.CallingConvention as ACC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Instruction as I
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Visibility as V
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "ExprGen.Function" $ do
  let wrapInFunction expr =
        AT.Function
          { AT.funcLoc = sampleLoc,
            AT.funcName = "test",
            AT.funcType = AT.TFunction AT.TVoid [] False,
            AT.funcParams = [],
            AT.funcBody =
              expr
          }
  H.describe "generateFunction" $ do
    H.it "should generate function without parametes" $ do
      let funcExpr =
            AT.Function
              { AT.funcLoc = sampleLoc,
                AT.funcName = "test",
                AT.funcType = AT.TFunction (AT.TInt 32) [] True,
                AT.funcParams = [],
                AT.funcBody =
                  AT.Block
                    [ AT.Return sampleLoc (Just (AT.Lit sampleLoc (AT.LInt 0)))
                    ]
              }
      let blocks = generateTestBlocks funcExpr
      length blocks `H.shouldBe` 1

    H.it "should generate function with void return type" $ do
      let funcExpr =
            AT.Function
              { AT.funcLoc = sampleLoc,
                AT.funcName = "test",
                AT.funcType = AT.TFunction AT.TVoid [] True,
                AT.funcParams = [],
                AT.funcBody =
                  AT.Block
                    []
              }
      let blocks = generateTestBlocks funcExpr
      length blocks `H.shouldBe` 1

    H.it "should generate function with parameters" $ do
      let funcExpr =
            AT.Function
              { AT.funcLoc = sampleLoc,
                AT.funcName = "test",
                AT.funcType = AT.TFunction (AT.TInt 32) [AT.TInt 32] False,
                AT.funcParams = ["x"],
                AT.funcBody =
                  AT.Block
                    [ AT.Return sampleLoc (Just (AT.Lit sampleLoc (AT.LInt 0)))
                    ]
              }
      let blocks = generateTestBlocks funcExpr
      length blocks `H.shouldBe` 1

    H.it "should generate correct global function definition" $ do
      let funcExpr =
            AT.Function
              { AT.funcLoc = sampleLoc,
                AT.funcName = "test",
                AT.funcType = AT.TFunction (AT.TInt 32) [AT.TBoolean, AT.TFunction (AT.TInt 32) [AT.TInt 32] False, AT.TFunction (AT.TInt 32) [AT.TInt 32] False] False,
                AT.funcParams = ["bool", "x", "y"],
                AT.funcBody =
                  AT.Block
                    [ AT.Return sampleLoc (Just (AT.Lit sampleLoc (AT.LInt 0)))
                    ]
              }
      let result = generateModule [("test", funcExpr)]
      case result of
        Left _ -> H.expectationFailure "Expected a module, but codegen failed"
        Right module' -> do
          let globals = getDefinitions module'

          length globals `H.shouldBe` 1

          G.linkage (head globals) `H.shouldBe` L.External
          G.visibility (head globals) `H.shouldBe` V.Default
          G.dllStorageClass (head globals) `H.shouldBe` Nothing
          G.callingConvention (head globals) `H.shouldBe` ACC.C
          G.returnAttributes (head globals) `H.shouldBe` []
          G.returnType (head globals)
            `H.shouldBe` T.IntegerType
              { T.typeBits = 32
              }
          G.name (head globals) `H.shouldBe` AST.Name (U.stringToByteString "test")
          G.parameters (head globals)
            `H.shouldBe` ( [ G.Parameter
                               T.IntegerType
                                 { T.typeBits = 1
                                 }
                               (AST.Name (U.stringToByteString "bool_0"))
                               [],
                             G.Parameter
                               T.PointerType
                                 { T.pointerReferent =
                                     T.FunctionType
                                       { T.resultType =
                                           T.IntegerType
                                             { T.typeBits = 32
                                             },
                                         T.argumentTypes =
                                           [ T.IntegerType
                                               { T.typeBits = 32
                                               }
                                           ],
                                         T.isVarArg = False
                                       },
                                   T.pointerAddrSpace = AS.AddrSpace 0
                                 }
                               (AST.Name (U.stringToByteString "x_0"))
                               [],
                             G.Parameter
                               T.PointerType
                                 { T.pointerReferent =
                                     T.FunctionType
                                       { T.resultType =
                                           T.IntegerType
                                             { T.typeBits = 32
                                             },
                                         T.argumentTypes =
                                           [ T.IntegerType
                                               { T.typeBits = 32
                                               }
                                           ],
                                         T.isVarArg = False
                                       },
                                   T.pointerAddrSpace = AS.AddrSpace 0
                                 }
                               (AST.Name (U.stringToByteString "y_0"))
                               []
                           ],
                           False
                         )
          G.functionAttributes (head globals) `H.shouldBe` []
          G.section (head globals) `H.shouldBe` Nothing
          G.comdat (head globals) `H.shouldBe` Nothing
          G.alignment (head globals) `H.shouldBe` 0
          G.garbageCollectorName (head globals) `H.shouldBe` Nothing
          G.prefix (head globals) `H.shouldBe` Nothing
          G.basicBlocks (head globals)
            `H.shouldBe` [ G.BasicBlock
                             (AST.UnName 0)
                             []
                             ( I.Do
                                 I.Ret
                                   { I.returnOperand =
                                       Just
                                         ( AST.ConstantOperand
                                             C.Int
                                               { C.integerBits = 32,
                                                 C.integerValue = 0
                                               }
                                         ),
                                     I.metadata' = []
                                   }
                             )
                         ]
          G.personalityFunction (head globals) `H.shouldBe` Nothing
          G.metadata (head globals) `H.shouldBe` []

    H.it "should throw error if function type is not valid" $ do
      let funcExpr =
            AT.Function
              { AT.funcLoc = sampleLoc,
                AT.funcName = "test",
                AT.funcType = AT.TInt 32,
                AT.funcParams = ["bool", "x", "y"],
                AT.funcBody =
                  AT.Block
                    [ AT.Return sampleLoc (Just (AT.Lit sampleLoc (AT.LInt 0)))
                    ]
              }
          expectedError = CE.CodegenError sampleLoc $ CE.UnsupportedDefinition funcExpr
      testError expectedError funcExpr

    H.describe "generateFunction" $ do
      H.it "should generate correct foreign function definiton" $ do
        let funcExpr =
              AT.ForeignFunction
                { AT.funcLoc = sampleLoc,
                  AT.funcName = "test",
                  AT.funcType = AT.TFunction (AT.TInt 32) [AT.TBoolean, AT.TFunction (AT.TInt 32) [AT.TInt 32] False, AT.TFunction (AT.TInt 32) [AT.TInt 32] False] False
                }
        let result = generateModule [("test", funcExpr)]
        case result of
          Left error' -> H.expectationFailure $ show error'
          Right module' -> do
            let globals = getDefinitions module'

            length globals `H.shouldBe` 1

            G.linkage (head globals) `H.shouldBe` L.External
            G.visibility (head globals) `H.shouldBe` V.Default
            G.dllStorageClass (head globals) `H.shouldBe` Nothing
            G.callingConvention (head globals) `H.shouldBe` ACC.C
            G.returnAttributes (head globals) `H.shouldBe` []
            G.returnType (head globals)
              `H.shouldBe` T.IntegerType
                { T.typeBits = 32
                }
            G.name (head globals) `H.shouldBe` AST.Name (U.stringToByteString "test")
            G.parameters (head globals)
              `H.shouldBe` ( [ G.Parameter
                                 T.IntegerType
                                   { T.typeBits = 1
                                   }
                                 (AST.Name (U.stringToByteString ""))
                                 [],
                               G.Parameter
                                 T.FunctionType
                                   { T.resultType =
                                       T.IntegerType
                                         { T.typeBits = 32
                                         },
                                     T.argumentTypes =
                                       [ T.IntegerType
                                           { T.typeBits = 32
                                           }
                                       ],
                                     T.isVarArg = False
                                   }
                                 (AST.Name (U.stringToByteString ""))
                                 [],
                               G.Parameter
                                 T.FunctionType
                                   { T.resultType =
                                       T.IntegerType
                                         { T.typeBits = 32
                                         },
                                     T.argumentTypes =
                                       [ T.IntegerType
                                           { T.typeBits = 32
                                           }
                                       ],
                                     T.isVarArg = False
                                   }
                                 (AST.Name (U.stringToByteString ""))
                                 []
                             ],
                             False
                           )

      H.it "should throw error if foreign function is not valir" $ do
        let funcExpr =
              AT.ForeignFunction
                { AT.funcLoc = sampleLoc,
                  AT.funcName = "test",
                  AT.funcType = AT.TInt 32
                }
            expectedError = CE.CodegenError sampleLoc $ CE.UnsupportedDefinition funcExpr
        testError expectedError funcExpr

    H.describe "generateFunctionCall" $ do
      H.it "should generate function call" $ do
        let funcExpr =
              AT.Function
                { AT.funcLoc = sampleLoc,
                  AT.funcName = "test",
                  AT.funcType = AT.TFunction (AT.TInt 32) [AT.TInt 32] False,
                  AT.funcParams = ["x"],
                  AT.funcBody =
                    AT.Block
                      [ AT.Call sampleLoc (AT.Var sampleLoc "test" (AT.TFunction (AT.TInt 32) [AT.TInt 32] False)) [AT.Lit sampleLoc (AT.LInt 0)],
                        AT.Return sampleLoc (Just (AT.Lit sampleLoc (AT.LInt 0)))
                      ]
                }
        let blocks = generateTestBlocks funcExpr
        let instrs = getInstructions blocks

        length blocks `H.shouldBe` 1

        case L.find isCallInstr instrs of
          Just (AST.UnName _ AST.:= AST.Call {AST.tailCallKind = tc, AST.callingConvention = c, AST.returnAttributes = r, AST.function = f, AST.arguments = a, AST.functionAttributes = fa}) -> do
            tc `H.shouldBe` Nothing
            c `H.shouldBe` ACC.C
            r `H.shouldBe` []
            f
              `H.shouldBe` Right
                ( AST.ConstantOperand
                    ( C.GlobalReference
                        ( T.PointerType
                            { T.pointerReferent =
                                T.FunctionType
                                  { T.resultType =
                                      T.IntegerType
                                        { T.typeBits = 32
                                        },
                                    T.argumentTypes =
                                      [ T.IntegerType
                                          { T.typeBits = 32
                                          }
                                      ],
                                    T.isVarArg = False
                                  },
                              T.pointerAddrSpace = AS.AddrSpace 0
                            }
                        )
                        (AST.Name (U.stringToByteString "test"))
                    )
                )
            a
              `H.shouldBe` [ ( AST.ConstantOperand
                                 ( C.Int
                                     { C.integerBits = 32,
                                       C.integerValue = 0
                                     }
                                 ),
                               []
                             )
                           ]
            fa `H.shouldBe` []
          _ -> H.expectationFailure "Expected a BitCast instruction"

    H.it "should throw error if function called does not exist" $ do
      let callExpr =
            AT.Call
              { AT.callLoc = sampleLoc,
                AT.callFunc = AT.Var sampleLoc "NotAFunc" (AT.TFunction (AT.TInt 32) [AT.TInt 32] False),
                AT.callArgs = [AT.Lit sampleLoc (AT.LInt 0)]
              }

          expectedError = CE.CodegenError sampleLoc $ CE.UnsupportedFunctionCall "NotAFunc"
      testError expectedError (wrapInFunction callExpr)

    H.it "should throw error if function call type is not valid" $ do
      let callExpr =
            AT.Call
              { AT.callLoc = sampleLoc,
                AT.callFunc = AT.Lit sampleLoc (AT.LInt 0),
                AT.callArgs = [AT.Lit sampleLoc (AT.LInt 0)]
              }
          expectedError = CE.CodegenError sampleLoc $ CE.UnsupportedDefinition callExpr
      testError expectedError (wrapInFunction callExpr)
  where
    sampleLoc = AT.SrcLoc "test.c" 1 1

    testError :: CE.CodegenError -> AT.Expr -> H.Expectation
    testError expectedError expr = do
      let testProg = AT.Program [("test", expr)] [] "test.c"
      let result = CC.codegen testProg
      case result of
        Left error' -> do
          error' `H.shouldBe` expectedError
        Right _ -> H.expectationFailure "Expected a CodegenError, but codegen succeeded"

    generateTestBlocks expr = case CC.codegen testProg of
      Right mod' -> concatMap G.basicBlocks $ getDefinitions mod'
      Left _ -> []
      where
        testProg = AT.Program [("test", expr)] [] "test.c"

    generateModule :: [(String, AT.Expr)] -> Either CE.CodegenError AST.Module
    generateModule expr = CC.codegen testProg
      where
        testProg = AT.Program expr [] "test.c"

    getDefinitions mod' =
      [f | AST.GlobalDefinition f@(AST.Function {}) <- AST.moduleDefinitions mod']

    getInstructions blocks =
      [i | G.BasicBlock _ instrs _ <- blocks, i <- instrs]

    isCallInstr (AST.UnName _ AST.:= AST.Call {}) = True
    isCallInstr _ = False
