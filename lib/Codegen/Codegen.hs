module Codegen.Codegen where

import qualified Ast.Types as AT
import qualified Codegen.Errors as CE
import qualified Codegen.ExprGen.ExprGen ()
import qualified Codegen.ExprGen.Global as EG
import qualified Codegen.State as CS
import qualified Codegen.Utils as U
import qualified Control.Monad.Except as E
import qualified Control.Monad.State as S
import qualified LLVM.AST as AST
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as IRM

-- | Generate LLVM code for a program.
codegen :: AT.Program -> Either CE.CodegenError AST.Module
codegen program =
  E.runExcept $
    M.buildModuleT (U.stringToByteString $ AT.sourceFile program) $
      IRM.runIRBuilderT IRM.emptyIRBuilder $
        S.evalStateT
          (mapM_ (EG.generateGlobal . snd) (AT.globals program))
          (CS.CodegenState [] [] Nothing [] 0)
