module Codegen.ExprGen.ControlFlowSpec (spec) where

import qualified Ast.Types as AT
import qualified Codegen.Codegen as C
import qualified Test.Hspec as H

simpleSum :: AT.Program
simpleSum =
  AT.Program
    { AT.globals = [("main", mainDef)],
      AT.types = [],
      AT.sourceFile = "sample.c"
    }
  where
    mainDef =
      AT.Function
        { AT.funcLoc =
            AT.SrcLoc
              { AT.srcFile = "examples/putstr.ff",
                AT.srcLine = 35,
                AT.srcCol = 1
              },
          AT.funcName = "main",
          AT.funcType =
            AT.TFunction
              { AT.returnType = AT.TInt 32,
                AT.paramTypes = [],
                AT.isVariadic = False
              },
          AT.funcParams = [],
          AT.funcBody =
            AT.Block
              [ AT.Declaration
                  { AT.declLoc =
                      AT.SrcLoc
                        { AT.srcFile = "examples/putstr.ff",
                          AT.srcLine = 35,
                          AT.srcCol = 1
                        },
                    AT.declName = "myInt",
                    AT.declType = AT.TInt 32,
                    AT.declInit =
                      Just
                        ( AT.Lit
                            ( AT.SrcLoc
                                { AT.srcFile = "examples/putstr.ff",
                                  AT.srcLine = 35,
                                  AT.srcCol = 1
                                }
                            )
                            (AT.LInt 0)
                        )
                  },
                AT.Assignment
                  { AT.assignLoc =
                      AT.SrcLoc
                        { AT.srcFile = "examples/putstr.ff",
                          AT.srcLine = 35,
                          AT.srcCol = 1
                        },
                    AT.assignTarget =
                      AT.Var
                        ( AT.SrcLoc
                            { AT.srcFile = "examples/putstr.ff",
                              AT.srcLine = 35,
                              AT.srcCol = 1
                            }
                        )
                        "myInt"
                        (AT.TInt 32),
                    AT.assignValue =
                      AT.Lit
                        ( AT.SrcLoc
                            { AT.srcFile = "examples/putstr.ff",
                              AT.srcLine = 35,
                              AT.srcCol = 1
                            }
                        )
                        (AT.LInt 12)
                  },
                AT.From
                  { AT.fromLoc =
                      AT.SrcLoc
                        { AT.srcFile = "examples/putstr.ff",
                          AT.srcLine = 35,
                          AT.srcCol = 1
                        },
                    AT.fromStart =
                      AT.Lit
                        ( AT.SrcLoc
                            { AT.srcFile = "examples/putstr.ff",
                              AT.srcLine = 35,
                              AT.srcCol = 1
                            }
                        )
                        (AT.LInt 0),
                    AT.fromEnd =
                      AT.Var
                        ( AT.SrcLoc
                            { AT.srcFile = "examples/putstr.ff",
                              AT.srcLine = 35,
                              AT.srcCol = 1
                            }
                        )
                        "myInt"
                        (AT.TInt 32),
                    AT.fromStep =
                      AT.Assignment
                        { AT.assignLoc =
                            AT.SrcLoc
                              { AT.srcFile = "examples/putstr.ff",
                                AT.srcLine = 35,
                                AT.srcCol = 1
                              },
                          AT.assignTarget =
                            AT.Var
                              ( AT.SrcLoc
                                  { AT.srcFile = "examples/putstr.ff",
                                    AT.srcLine = 35,
                                    AT.srcCol = 1
                                  }
                              )
                              "i"
                              (AT.TInt 32),
                          AT.assignValue =
                            AT.Op
                              ( AT.SrcLoc
                                  { AT.srcFile = "examples/putstr.ff",
                                    AT.srcLine = 35,
                                    AT.srcCol = 1
                                  }
                              )
                              AT.Add
                              ( AT.Var
                                  ( AT.SrcLoc
                                      { AT.srcFile = "examples/putstr.ff",
                                        AT.srcLine = 35,
                                        AT.srcCol = 1
                                      }
                                  )
                                  "i"
                                  (AT.TInt 32)
                              )
                              ( AT.Lit
                                  ( AT.SrcLoc
                                      { AT.srcFile = "examples/putstr.ff",
                                        AT.srcLine = 35,
                                        AT.srcCol = 1
                                      }
                                  )
                                  (AT.LInt 1)
                              )
                        },
                    AT.fromVar =
                      AT.Declaration
                        { AT.declLoc =
                            AT.SrcLoc
                              { AT.srcFile = "examples/putstr.ff",
                                AT.srcLine = 35,
                                AT.srcCol = 1
                              },
                          AT.declName = "i",
                          AT.declType = AT.TInt 32,
                          AT.declInit =
                            Just
                              ( AT.Lit
                                  ( AT.SrcLoc
                                      { AT.srcFile = "examples/putstr.ff",
                                        AT.srcLine = 35,
                                        AT.srcCol = 1
                                      }
                                  )
                                  (AT.LInt 0)
                              )
                        },
                    AT.fromBody =
                      AT.Block
                        [ AT.If
                            { AT.ifLoc =
                                AT.SrcLoc
                                  { AT.srcFile = "examples/putstr.ff",
                                    AT.srcLine = 35,
                                    AT.srcCol = 1
                                  },
                              AT.ifCond =
                                AT.Op
                                  ( AT.SrcLoc
                                      { AT.srcFile = "examples/putstr.ff",
                                        AT.srcLine = 35,
                                        AT.srcCol = 1
                                      }
                                  )
                                  AT.Eq
                                  ( AT.Op
                                      ( AT.SrcLoc
                                          { AT.srcFile = "examples/putstr.ff",
                                            AT.srcLine = 35,
                                            AT.srcCol = 1
                                          }
                                      )
                                      AT.Mod
                                      ( AT.Var
                                          ( AT.SrcLoc
                                              { AT.srcFile = "examples/putstr.ff",
                                                AT.srcLine = 35,
                                                AT.srcCol = 1
                                              }
                                          )
                                          "i"
                                          (AT.TInt 32)
                                      )
                                      ( AT.Lit
                                          ( AT.SrcLoc
                                              { AT.srcFile = "examples/putstr.ff",
                                                AT.srcLine = 35,
                                                AT.srcCol = 1
                                              }
                                          )
                                          (AT.LInt 2)
                                      )
                                  )
                                  ( AT.Lit
                                      ( AT.SrcLoc
                                          { AT.srcFile = "examples/putstr.ff",
                                            AT.srcLine = 35,
                                            AT.srcCol = 1
                                          }
                                      )
                                      (AT.LInt 0)
                                  ),
                              AT.ifThen =
                                AT.Block
                                  [ AT.Continue
                                      ( AT.SrcLoc
                                          { AT.srcFile = "examples/putstr.ff",
                                            AT.srcLine = 35,
                                            AT.srcCol = 1
                                          }
                                      )
                                  ],
                              AT.ifElse = Nothing
                            },
                          AT.If
                            { AT.ifLoc =
                                AT.SrcLoc
                                  { AT.srcFile = "examples/putstr.ff",
                                    AT.srcLine = 35,
                                    AT.srcCol = 1
                                  },
                              AT.ifCond =
                                AT.Op
                                  ( AT.SrcLoc
                                      { AT.srcFile = "examples/putstr.ff",
                                        AT.srcLine = 35,
                                        AT.srcCol = 1
                                      }
                                  )
                                  AT.Eq
                                  ( AT.Var
                                      ( AT.SrcLoc
                                          { AT.srcFile = "examples/putstr.ff",
                                            AT.srcLine = 35,
                                            AT.srcCol = 1
                                          }
                                      )
                                      "i"
                                      (AT.TInt 32)
                                  )
                                  ( AT.Lit
                                      ( AT.SrcLoc
                                          { AT.srcFile = "examples/putstr.ff",
                                            AT.srcLine = 35,
                                            AT.srcCol = 1
                                          }
                                      )
                                      (AT.LInt 5)
                                  ),
                              AT.ifThen =
                                AT.Block
                                  [ AT.Break
                                      ( AT.SrcLoc
                                          { AT.srcFile = "examples/putstr.ff",
                                            AT.srcLine = 35,
                                            AT.srcCol = 1
                                          }
                                      )
                                  ],
                              AT.ifElse = Nothing
                            }
                        ]
                  },
                AT.Declaration
                  { AT.declLoc =
                      AT.SrcLoc
                        { AT.srcFile = "examples/putstr.ff",
                          AT.srcLine = 35,
                          AT.srcCol = 1
                        },
                    AT.declName = "count",
                    AT.declType = AT.TInt 32,
                    AT.declInit =
                      Just
                        ( AT.Lit
                            ( AT.SrcLoc
                                { AT.srcFile = "examples/putstr.ff",
                                  AT.srcLine = 35,
                                  AT.srcCol = 1
                                }
                            )
                            (AT.LInt 0)
                        )
                  },
                AT.While
                  { AT.whileLoc =
                      AT.SrcLoc
                        { AT.srcFile = "examples/putstr.ff",
                          AT.srcLine = 35,
                          AT.srcCol = 1
                        },
                    AT.whileCond =
                      AT.Op
                        ( AT.SrcLoc
                            { AT.srcFile = "examples/putstr.ff",
                              AT.srcLine = 35,
                              AT.srcCol = 1
                            }
                        )
                        AT.Lt
                        ( AT.Var
                            ( AT.SrcLoc
                                { AT.srcFile = "examples/putstr.ff",
                                  AT.srcLine = 35,
                                  AT.srcCol = 1
                                }
                            )
                            "count"
                            (AT.TInt 32)
                        )
                        ( AT.Var
                            ( AT.SrcLoc
                                { AT.srcFile = "examples/putstr.ff",
                                  AT.srcLine = 35,
                                  AT.srcCol = 1
                                }
                            )
                            "myInt"
                            (AT.TInt 32)
                        ),
                    AT.whileBody =
                      AT.Block
                        [ AT.Assignment
                            { AT.assignLoc =
                                AT.SrcLoc
                                  { AT.srcFile = "examples/putstr.ff",
                                    AT.srcLine = 35,
                                    AT.srcCol = 1
                                  },
                              AT.assignTarget =
                                AT.Var
                                  ( AT.SrcLoc
                                      { AT.srcFile = "examples/putstr.ff",
                                        AT.srcLine = 35,
                                        AT.srcCol = 1
                                      }
                                  )
                                  "count"
                                  (AT.TInt 32),
                              AT.assignValue =
                                AT.Op
                                  ( AT.SrcLoc
                                      { AT.srcFile = "examples/putstr.ff",
                                        AT.srcLine = 35,
                                        AT.srcCol = 1
                                      }
                                  )
                                  AT.Add
                                  ( AT.Var
                                      ( AT.SrcLoc
                                          { AT.srcFile = "examples/putstr.ff",
                                            AT.srcLine = 35,
                                            AT.srcCol = 1
                                          }
                                      )
                                      "count"
                                      (AT.TInt 32)
                                  )
                                  ( AT.Lit
                                      ( AT.SrcLoc
                                          { AT.srcFile = "examples/putstr.ff",
                                            AT.srcLine = 35,
                                            AT.srcCol = 1
                                          }
                                      )
                                      (AT.LInt 1)
                                  )
                            }
                        ]
                  },
                AT.Return
                  ( AT.SrcLoc
                      { AT.srcFile = "examples/putstr.ff",
                        AT.srcLine = 35,
                        AT.srcCol = 1
                      }
                  )
                  ( Just
                      ( AT.Lit
                          ( AT.SrcLoc
                              { AT.srcFile = "examples/putstr.ff",
                                AT.srcLine = 35,
                                AT.srcCol = 1
                              }
                          )
                          (AT.LInt 0)
                      )
                  )
              ]
        }

spec :: H.Spec
spec = H.describe "Codegen" $ do
  let withModule test = do
        case C.codegen simpleSum of
          Left err ->
            H.expectationFailure $ "Failed to generate module: " ++ show err
          Right mod' -> test mod'

  H.it "should generate a valid module with main function" $ withModule $ \mod' -> do
    mod' `H.shouldBe` mod'
