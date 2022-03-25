{-|
  Description: Parser and types for Nix expressions (will be superseded by hnix in due time)
  -}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixExpr
  ( parseNixFile
  , parseNixString
  , NixExpr(..)
  , evalSymbols
  , prettyPrintSingleLine
  , NixFunction(NixFunction)
  , writeNixFile
  )
where

import           Control.Applicative        (empty)
import           Control.Lens               (makeLenses, makePrisms, to,
                                             traversed, (^.), (^..))
import           Control.Monad              (void)
import           Data.Bifunctor             (first)
import           Data.Functor               (($>))
import           Data.Generics.Labels       ()
import           Data.Map.Strict            (Map, fromList, toList)
import           Data.Text                  (Text, intercalate, pack, replace,
                                             unwords)
import           Data.Text.IO               (readFile, writeFile)
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           NixManager.Util            (TextualError, parseSafe, showText)
import           Prelude                    hiding (readFile, unwords,
                                             writeFile)
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist)
import           System.FilePath            (dropFileName)
import           Text.Megaparsec            (Parsec, errorBundlePretty, many,
                                             manyTill, parse, sepBy, try, (<?>),
                                             (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar,
                                             space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- | All information pertaining to Nix functions. Arguments are simplified to strings here, since we don’t need more, currently.
data NixFunction = NixFunction {
    functionArgs :: [Text]
  , functionExpr :: NixExpr
  } deriving(Show, Generic)

-- | Type for Nix expressions
data NixExpr = NixList [NixExpr] -- ^ Nix list, as in @[a b c]@
             | NixSet (Map Text NixExpr) -- ^ Nix attribute set, as in @{ foo = bar; }@. Keys are simplified to 'Text' until hnix arrives.
             | NixFunctionDecl NixFunction -- ^ Nix function declaration
             | NixSymbol Text -- ^ Nix symbols
             | NixString Text -- ^ Nix strings
             | NixBoolean Bool -- ^ Nix booleans
             | NixInt Integer -- ^ Nix integers (is unbounded justified?)
             | NixFloat Double -- ^ Nix floating point values
             | NixNull -- ^ Nix null value
             deriving(Show, Generic)

-- | Extract all symbols from a Nix expression (reads all the packages, for example)
evalSymbols :: NixExpr -> [Text]
evalSymbols (NixSymbol r ) = [r]
evalSymbols (NixList   rs) = concatMap evalSymbols rs
evalSymbols _              = []

-- | Pretty print a Nix expression as a single line (to be used in the GUI text entry fields)
prettyPrintSingleLine :: NixExpr -> Text
prettyPrintSingleLine = replace "\n" "" . prettyPrint

-- | Pretty print (or rather, serialize) a Nix expression
prettyPrint :: NixExpr -> Text
prettyPrint NixNull            = "null"
prettyPrint (NixFloat   f    ) = showText f
prettyPrint (NixInt     f    ) = showText f
prettyPrint (NixBoolean True ) = "true"
prettyPrint (NixBoolean False) = "false"
prettyPrint (NixString  s    ) = "\"" <> s <> "\""
prettyPrint (NixSymbol  s    ) = s
prettyPrint (NixFunctionDecl fn) =
  "{ "
    <> intercalate "," (fn ^. #functionArgs)
    <> " }: "
    <> (fn ^. #functionExpr . to prettyPrint)
prettyPrint (NixList xs) =
  "[ " <> unwords (xs ^.. traversed . to prettyPrint) <> " ]"
prettyPrint (NixSet m) =
  "{\n"
    <> foldMap (\(k, v) -> "  " <> k <> " = " <> prettyPrint v <> ";\n")
               (toList m)
    <> "}"

-- | Parsec type for the parser.
type Parser = Parsec Void Text

-- | Parser for a line comment
lineComment :: Parser ()
lineComment = L.skipLineComment "#"

-- | Space parser
sc :: Parser ()
sc = L.space space1 lineComment empty

-- | Lexeme parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Unsigned float parser (uses @Double@, besides the name)
float :: Parser Double
float = lexeme L.float

-- | Integer parser
integer :: Parser Integer
integer = lexeme L.decimal

-- | Signed integer parser
signedInteger :: Parser Integer
signedInteger = L.signed sc integer

-- | Signed float parser
signedFloat :: Parser Double
signedFloat = L.signed sc float

-- | Parser for a string literal. Possibly doesn’t understand escaping well enough. Needs testing or cargo-culting hnix.
stringLiteral :: Parser String
stringLiteral =
  (char '\"' *> manyTill L.charLiteral (char '\"')) <?> "string literal"

-- | Parser for a Nix list
listParser :: Parser NixExpr
listParser = do
  void (lexeme (char '['))
  exprs <- many (lexeme exprParser)
  void (char ']')
  pure (NixList exprs)

-- | Parser for a Nix string
stringParser :: Parser NixExpr
stringParser = NixString . pack <$> stringLiteral

-- | Parser for a Nix boolean
boolParser :: Parser NixExpr
boolParser =
  ((string "true" $> NixBoolean True) <|> (string "false" $> NixBoolean False))
    <?> "boolean"

-- | Parser for a Nix integer
intParser :: Parser NixExpr
intParser = NixInt <$> signedInteger <?> "int"

-- | Parser for a Nix function declaration (only supports “simple” arguments right now)
functionDeclParser :: Parser NixExpr
functionDeclParser = do
  void (lexeme (char '{'))
  symbols <- lexeme symbolParser' `sepBy` lexeme (char ',')
  void (lexeme (char '}'))
  void (lexeme (char ':'))
  NixFunctionDecl . NixFunction symbols <$> exprParser

-- | Parser for a Nix floating point number
floatParser :: Parser NixExpr
floatParser = NixFloat <$> signedFloat <?> "float"

-- | Parser for a Nix null value
nullParser :: Parser NixExpr
nullParser = (string "null" $> NixNull) <?> "null"

-- | Parses a single symbol character. Can contain dashes and dots currently (this is hacky, I know).
symbolChar :: Parser Char
symbolChar = alphaNumChar <|> char '.' <|> char '-'

-- | Parses a symbol (as a 'Text')
symbolParser' :: Parser Text
symbolParser' =
  string "..." <|> (pack <$> ((:) <$> letterChar <*> many symbolChar))

-- | Parses a Nix symbol
symbolParser :: Parser NixExpr
symbolParser = NixSymbol <$> symbolParser' <?> "symbol"

-- | Parses a Nix set (using simple keys for now, using the 'symbolParser')
setParser :: Parser NixExpr
setParser = do
  void (lexeme (char '{'))
  let bindingParser = do
        key <- lexeme symbolParser' <?> "set key"
        void (lexeme (char '='))

        value <- exprParser <?> "set value"
        void (lexeme (char ';'))
        pure (key, value)
  bindings <- fromList <$> many bindingParser
  void (lexeme (char '}'))
  pure (NixSet bindings)

-- | Parser for a Nix expression
exprParser :: Parser NixExpr
exprParser =
  listParser
    <|> try functionDeclParser
    <|> setParser
    <|> stringParser
    <|> nullParser
    <|> boolParser
    <|> try floatParser
    <|> try intParser
    <|> symbolParser

-- | Parses a 'Text' into a Nix expression (or an error)
parseNixString :: Text -> TextualError NixExpr
parseNixString = parseSafe exprParser "string expression"

-- | Parses a file (which may not exist, in which case a default is returned), returns the containing Nix expression or an error.
parseNixFile :: FilePath -> NixExpr -> IO (TextualError NixExpr)
parseNixFile fn defExpr = do
  exists <- doesFileExist fn
  if exists
    then parseSafe exprParser fn <$> readFile fn
    else pure (pure defExpr)

-- | Write a Nix expression to a file, creating the corresponding directory if it’s missing.
writeNixFile :: FilePath -> NixExpr -> IO ()
writeNixFile fp e = do
  createDirectoryIfMissing True (dropFileName fp)
  writeFile fp (prettyPrint e)
