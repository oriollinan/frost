{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Ast.Parser (parse) where

import qualified Ast.Tokenizer as T
import Ast.Types (AST (..), Expr)
import Control.Applicative (Alternative ((<|>)))
import Data.Foldable (Foldable (toList))
import qualified Data.Void as V
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.State as MST
import qualified Text.Megaparsec.Stream as MS

newtype TokenStream = TokenStream [T.Token]
  deriving (Show, Eq, Ord)

instance MS.Stream TokenStream where
  type Token TokenStream = T.Token
  type Tokens TokenStream = [T.Token]

  tokenToChunk _ t = [t]

  tokensToChunk _ ts = ts

  chunkToTokens _ chunk = chunk

  chunkLength _ = length

  chunkEmpty _ = null

  take1_ (TokenStream []) = Nothing
  take1_ (TokenStream (x : xs)) = Just (x, TokenStream xs)

  takeN_ n (TokenStream s)
    | null s = Nothing
    | n <= 0 = Just ([], TokenStream s)
    | otherwise = Just (take n s, TokenStream $ drop n s)

  takeWhile_ f (TokenStream s) =
    let (h, t) = span f s
     in (h, TokenStream t)

instance MS.VisualStream TokenStream where
  showTokens _ ts = unwords $ map show $ toList ts

instance MS.TraversableStream TokenStream where
  reachOffset o pst =
    let (TokenStream s) = MST.pstateInput pst
        oldOffset = MST.pstateOffset pst
        diff = o - oldOffset
        (_, rest) = splitAt diff s
        newPosState =
          pst
            { MST.pstateInput = TokenStream rest,
              MST.pstateOffset = o
            }
        shownStream = show (take 10 rest)
     in (Just shownStream, newPosState)

type Parser = M.Parsec V.Void TokenStream

parse :: String -> Either String AST
parse input = case T.tokenize input of
  Left err -> Left err
  Right tokens -> case M.parse parseProgram "" (TokenStream tokens) of
    Left err -> Left (M.errorBundlePretty err)
    Right ast -> Right ast

parseProgram :: Parser AST
parseProgram = do
  exprs <- M.many parseExpr
  M.eof
  return $ AST exprs

parseExpr :: Parser Expr
parseExpr = parseAtom <|> parseList

parseAtom :: Parser Expr
parseAtom = fail "not defined"

parseList :: Parser Expr
parseList = fail "not defined"
