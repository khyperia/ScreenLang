module Parser (Parser.parse, addPrelude) where

import LLVM.General.AST.Type
import qualified LLVM.General.AST.AddrSpace as AddrSpace
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Data.List
import Data.Char
import Control.Applicative hiding ((<|>), many)
import Ast
import ScreenLangPrelude

-- Low level parsers

tokenSymbol :: String -> Parser String
tokenSymbol s = try (string s) <* spaces

tokenWord :: String -> Parser String
tokenWord s = try (string s) <* notFollowedBy alphaNum <* spaces

identifier :: Parser String
identifier = (((:) <$> letter <*> many alphaNum) <|> between (char '`') (char '`') (many1 (noneOf "`"))) <* spaces

-- http://stackoverflow.com/questions/10726085
pinteger :: Parser Integer
pinteger = (foldl' (\a i -> a * 10 + fromIntegral (digitToInt i)) 0 <$> many1 digit) <* spaces

pboolean :: Parser Bool
pboolean = (const True <$> tokenWord "true") <|> (const False <$> tokenWord "false")

-- Medium level parsers

parentheses :: Parser a -> Parser a
parentheses = between (tokenSymbol "(") (tokenSymbol ")")

commaList :: Parser a -> Parser [a]
commaList = flip sepBy (tokenSymbol ",")

parenCommas :: Parser a -> Parser [a]
parenCommas = parentheses . commaList

-- High level parsers

parseType :: Parser Type
parseType = IntegerType <$> (char 'i' *> (fromInteger <$> pinteger))
        <|> flip FloatingPointType IEEE <$> (char 'f' *> (fromInteger <$> pinteger))
        <|> flip PointerType (AddrSpace.AddrSpace 0) <$> (char 'p' *> parseType)
        <|> const VoidType <$> tokenWord "void"
        <|> StructureType False <$> parenCommas parseType

primary :: Parser Expression
primary = (ConstantInteger <$> pinteger)
      <|> (ConstantBoolean <$> pboolean)
      <|> (Identifier <$> identifier)
      <|> (CreateTuple <$> (tokenSymbol "@" *> parseType) <*> parenCommas parseExpr)
      <|> parentheses parseExpr

parseExpr :: Parser Expression
parseExpr = buildExpressionParser table primary
  where table = [[Postfix (flip MethodCall <$> parenCommas parseExpr)]
                ,[prefix "-" Negation]
                ,[binary "*" Multiplication AssocLeft, binary "/" Division AssocLeft]
                ,[binary "+" Addition AssocLeft, binary "-" Subtraction AssocLeft]
                ,[binary "<<" ShiftLeft AssocLeft, binary ">>" ShiftRight AssocLeft]
                ,[binary "&" And AssocLeft]
                ,[binary "|" Or AssocLeft]
                ,[binary "=" Assignment AssocRight]
                ]
        binary name fun = Infix (tokenSymbol name >> return fun)
        prefix name fun = Prefix (tokenSymbol name >> return fun)

-- docs because lump of code below is con-foo-zing
-- parseIf := if /expression/ then /statements/ else /statements/ end | if /expression/ /statement/
parseIf :: Parser Statement
parseIf = IfStatement <$> (tokenWord "if" *> parseExpr) <*> parseBlock <*> ((tokenWord "else" *> parseBlock) <|> return [])

parseWhile :: Parser Statement
parseWhile = WhileStatement <$> (tokenWord "while" *> parseExpr) <*> parseBlock

parseReturn :: Parser Statement
parseReturn = (Return . Just) <$> (tokenWord "return" *> parseExpr)

attributes :: Parser [Attributes]
attributes = many attribute
  where attribute = const DllImport <$> tokenWord "dllimport" <|> const StdCall <$> tokenWord "stdcall"

parseFn :: Parser Statement
parseFn = Function <$> (tokenWord "fn" *> attributes) <*> parseType <*> identifier <*> parenCommas ((,) <$> parseType <*> identifier) <*> ((const Nothing <$> tokenSymbol ";") <|> (Just <$> parseBlock))

parseStatement :: Parser Statement
parseStatement = parseIf <|> parseWhile <|> parseReturn <|> parseFn <|> (ExprStatement <$> parseExpr)

parseBlock :: Parser Block
parseBlock = between (tokenSymbol "{") (tokenSymbol "}") (many parseStatement)
         <|> ((:[]) <$> parseStatement)

parser :: Parser Block
parser = many parseStatement <* eof

prelude :: Block
prelude = extract $ runParser parser () "Prelude" ScreenLangPrelude.prelude
  where extract (Left l) = error ("Parse error in prelude: " ++ show l)
        extract (Right r) = r

parse :: SourceName -> String -> Either ParseError Block
parse = runParser parser ()

addPrelude :: Block -> Block
addPrelude = (Parser.prelude++)
