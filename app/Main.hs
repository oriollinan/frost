{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Ast.Parser as P
import qualified Codegen.Codegen as C
import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Except as E
import qualified Data.Text.Lazy as TL
import qualified LLVM.Pretty as LLVM
import qualified Options.Applicative as O
import qualified System.Exit as EX
import qualified System.FilePath as FP
import qualified System.IO as S
import qualified Text.Pretty.Simple as PS

data CompileError
  = ParseError String
  | CodegenError String
  deriving (Show)

data Options = Options
  { input :: Maybe FilePath,
    out :: Maybe FilePath,
    verbose :: Bool
  }

optionsParser :: O.Parser Options
optionsParser =
  Options
    <$> O.optional
      ( O.strOption
          ( O.long "input"
              <> O.short 'i'
              <> O.metavar "FILENAME"
              <> O.help "Input file path"
          )
      )
    <*> O.optional
      ( O.strOption
          ( O.long "out"
              <> O.short 'o'
              <> O.metavar "FILENAME"
              <> O.help "Output file path"
          )
      )
    <*> O.switch
      ( O.long "verbose"
          <> O.short 'v'
          <> O.help "Enable verbose logging"
      )

optionsInfo :: O.ParserInfo Options
optionsInfo =
  O.info
    (O.helper <*> optionsParser)
    ( O.fullDesc
        <> O.progDesc "Compile FrostLang source code to LLVM IR"
        <> O.header "FrostLang Compiler"
    )

compile :: String -> String -> Bool -> E.ExceptT CompileError IO String
compile input source verbose = do
  result <- IO.liftIO $ P.parse input source
  ast <- case result of
    Left err -> E.throwE $ ParseError err
    Right res -> return res

  IO.liftIO $ logMsg verbose (TL.unpack $ PS.pShow ast)

  case C.codegen ast of
    Left err -> E.throwE $ CodegenError (TL.unpack $ PS.pShow err)
    Right lmod -> return $ TL.unpack $ LLVM.ppllvm lmod

logMsg :: Bool -> String -> IO ()
logMsg verbose msg = M.when verbose $ S.hPutStrLn S.stderr ("[LOG] " ++ msg)

handleError :: String -> String -> Bool -> IO a
handleError errType errMsg verbose = do
  S.hPutStrLn S.stderr $ "Error: " ++ errMsg
  logMsg verbose $ "Compilation failed during " ++ errType ++ "."
  EX.exitWith $ EX.ExitFailure 84

main :: IO ()
main = do
  Options {input, out, verbose} <- O.execParser optionsInfo

  (filePath, source) <- case input of
    Nothing -> do
      logMsg verbose "Reading from stdin..."
      source <- S.getContents
      return ("<stdin>", source)
    Just fp ->
      if FP.takeExtension fp /= ".ff"
        then handleError "file input" "Only .ff files are supported" verbose
        else do
          source <- readFile fp
          return (fp, source)

  logMsg verbose "Starting compilation..."

  result <- E.runExceptT $ compile filePath source verbose
  case result of
    Left (ParseError err) -> handleError "parsing" err verbose
    Left (CodegenError err) -> handleError "code generation" err verbose
    Right llvm -> case out of
      Just outFile -> do
        writeFile outFile llvm
        logMsg verbose $ "Compilation successful! Output written to: " ++ outFile
      Nothing -> do
        putStr llvm
        logMsg verbose "Compilation successful! Output written to stdout"
