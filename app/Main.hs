{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Ast.Parser as P
import qualified Codegen.Codegen as C
import qualified Control.Monad as M
import qualified Control.Monad.Trans.Except as E
import qualified Data.Text.Lazy as TL
import qualified LLVM.Pretty as LLVM
import qualified Options.Applicative as O
import qualified System.Exit as EX
import qualified System.IO as S

data CompileError
  = ParseError String
  | CodegenError String
  deriving (Show)

data Options = Options
  { input :: Maybe FilePath,
    out :: FilePath,
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
              <> O.help "Input file to read from (defaults to STDIN)"
          )
      )
    <*> O.strOption
      ( O.long "out"
          <> O.metavar "FILENAME"
          <> O.help "Output file for the generated LLVM IR"
          <> O.value "demo/generated.ll"
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
        <> O.progDesc "A compiler that generates LLVM IR from a scheme-like language"
        <> O.header "Scheme-to-LLVM Compiler"
    )

compile :: String -> E.ExceptT CompileError IO String
compile input = do
  ast <- case P.parse input of
    Left err -> E.throwE $ ParseError (show err)
    Right res -> return res

  case C.codegen ast of
    Left err -> E.throwE $ CodegenError (show err)
    Right lmod -> return $ TL.unpack $ LLVM.ppllvm lmod

logMsg :: Bool -> String -> IO ()
logMsg verbose msg = M.when verbose $ S.hPutStrLn S.stderr ("[LOG] " ++ msg)

readInput :: Maybe FilePath -> IO String
readInput Nothing = getContents
readInput (Just filePath) = readFile filePath

handleError :: String -> String -> Bool -> IO a
handleError errType errMsg verbose = do
  S.hPutStrLn S.stderr $ "Error: " ++ errMsg
  logMsg verbose $ "Compilation failed during " ++ errType ++ "."
  EX.exitWith $ EX.ExitFailure 84

main :: IO ()
main = do
  Options {input, out, verbose} <- O.execParser optionsInfo
  source <- readInput input

  logMsg verbose "Starting compilation..."
  result <- E.runExceptT $ compile source

  case result of
    Left (ParseError err) -> handleError "parsing" err verbose
    Left (CodegenError err) -> handleError "parsing" err verbose
    Right llvm -> do
      writeFile out llvm
      logMsg verbose $ "Compilation successful! Output written to: " ++ out
