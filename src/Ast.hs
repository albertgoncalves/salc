module Ast where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace)
import Data.Text (Text, pack)
import Parser
  ( Parser (..),
    Pos,
    char,
    end,
    many,
    many1,
    satisfy,
    string,
    (<$$>),
  )

data Op
  = OpDot
  | OpAdd
  | OpSub
  | OpLT
  deriving (Eq, Show)

data Expr
  = EBinOp (Pos Op) Expr Expr
  | EBool (Pos Bool)
  | ECall Expr [Expr]
  | EChar (Pos Char)
  | EFloat (Pos Float)
  | EIdent (Pos Text)
  | EInt (Pos Int)
  | EStr (Pos Text)
  | EUnOp (Pos Op) Expr
  deriving (Eq, Show)

data Type
  = TBool
  | TChar
  | TF32
  | TI32
  deriving (Eq, Show)

data Stmt
  = SBlock [Stmt]
  | SAssign Expr Expr
  | SBreak (Pos ())
  | SCont (Pos ())
  | SDecl (Pos Type) Expr (Maybe Expr)
  | SEffect Expr
  | SIf Expr [Stmt]
  | SIfElse Expr [Stmt] [Stmt]
  | SLoop [Stmt]
  | SRet Expr
  deriving (Eq, Show)

isNewline :: Char -> Bool
isNewline '\n' = True
isNewline _ = False

comment :: Parser (Pos ())
comment = void <$> (char '#' <* p1 <* p2)
  where
    p1 = many (satisfy $ not . isNewline)
    p2 = (void <$> satisfy isNewline) <|> end

space :: Parser (Pos ())
space = void . sequenceA <$> many1 (satisfy isSpace)

manySpaces :: Parser (Pos ())
manySpaces = void . sequenceA <$> many (space <|> comment)

digits :: Parser [Pos Char]
digits = many1 (satisfy isDigit)

decimal :: Parser [Pos Char]
decimal = (++) <$> digits <*> ((:) <$> char '.' <*> digits)

int :: Parser (Pos Int)
int = (read <$>) . sequenceA <$> digits

float :: Parser (Pos Float)
float = (read <$>) . sequenceA <$> decimal

bool :: Parser (Pos Bool)
bool = (const True <$$> string "true") <|> (const False <$$> string "false")

underscore :: Parser (Pos Char)
underscore = char '_'

ident :: Parser (Pos Text)
ident = (pack <$>) . sequenceA <$> ((:) <$> p1 <*> p2)
  where
    p1 = satisfy isLower <|> underscore
    p2 = many (satisfy isAlphaNum <|> underscore)

doubleQuote :: Parser (Pos Char)
doubleQuote = char '"'

stringLiteral :: Parser (Pos Text)
stringLiteral =
  (\(n, _) (_, x) -> (n, pack x))
    <$> doubleQuote <*> (sequenceA <$> p <* doubleQuote)
  where
    p = many $ (const '"' <$$> string "\\\"") <|> satisfy (/= '"')

singleQuote :: Parser (Pos Char)
singleQuote = char '\''

charLiteral :: Parser (Pos Char)
charLiteral = singleQuote *> (p1 <|> p2 <|> p3) <* singleQuote
  where
    p1 = const '\'' <$$> string "\\'"
    p2 = const '\n' <$$> string "\\n"
    p3 = satisfy (const True)

negative :: Parser (Pos Op)
negative = const OpSub <$$> char '-'

op :: Parser (Pos Op)
op =
  negative
    <|> (const OpDot <$$> char '.')
    <|> (const OpAdd <$$> char '+')
    <|> (const OpLT <$$> char '<')

parens :: Parser a -> Parser a
parens p = char '(' *> manySpaces *> p <* manySpaces <* char ')'

binOp :: Parser Expr
binOp = parens $ EBinOp <$> op <*> p <*> p
  where
    p = manySpaces *> expr

call :: Parser Expr
call = parens $ ECall <$> p1 <*> p2
  where
    p1 = string "call" *> manySpaces *> expr <* manySpaces
    p2 = many (expr <* manySpaces)

expr :: Parser Expr
expr =
  parens expr
    <|> EUnOp <$> (negative <* manySpaces) <*> expr
    <|> binOp
    <|> call
    <|> EBool <$> bool
    <|> EChar <$> charLiteral
    <|> EFloat <$> float
    <|> EIdent <$> ident
    <|> EInt <$> int
    <|> EStr <$> stringLiteral

semicolon :: Parser (Pos Char)
semicolon = manySpaces *> char ';'

assign :: Parser Stmt
assign = SAssign <$> p1 <*> p2
  where
    p1 = expr <* manySpaces <* char '=' <* manySpaces
    p2 = expr <* semicolon

block :: Parser [Stmt]
block = char '{' *> manySpaces *> many (stmt <* manySpaces) <* char '}'

ifElse :: Parser Stmt
ifElse = (SIfElse <$> p1 <*> block <*> (p2 *> p3)) <|> p4
  where
    p1 = string "if" *> manySpaces *> expr <* manySpaces
    p2 = manySpaces *> string "else" *> manySpaces
    p3 = block <|> (pure <$> ifElse) <|> (pure <$> p4)
    p4 = SIf <$> p1 <*> block

type' :: Parser (Pos Type)
type' =
  const TI32 <$$> string "i32"
    <|> const TF32 <$$> string "f32"
    <|> const TBool <$$> string "bool"
    <|> const TChar <$$> string "char"

decl :: Parser Stmt
decl = p4 <|> p5
  where
    p1 = type' <* manySpaces
    p2 = expr <* manySpaces <* char '=' <* manySpaces
    p3 = expr <* semicolon
    p4 = SDecl <$> p1 <*> p3 <*> pure Nothing
    p5 = SDecl <$> p1 <*> p2 <*> (Just <$> p3)

stmt :: Parser Stmt
stmt =
  SBlock <$> block
    <|> SBreak <$> (void <$> string "break" <* semicolon)
    <|> SCont <$> (void <$> string "continue" <* semicolon)
    <|> SRet <$> (string "return" *> manySpaces *> expr <* semicolon)
    <|> SLoop <$> (string "loop" *> manySpaces *> block)
    <|> ifElse
    <|> assign
    <|> decl
    <|> SEffect <$> expr <* semicolon

program :: Parser [Stmt]
program = manySpaces *> many (stmt <* manySpaces) <* end
