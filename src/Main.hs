{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Ast (Stmt, isNewline, program)
import Data.Text (Text, uncons)
import Data.Text.IO (readFile)
import Parser (Consumed (..), Input (..), parse)
import System.Environment (getArgs)
import Prelude hiding (readFile)

newtype Line = Line Int
  deriving (Num, Show)

newtype Col = Col Int
  deriving (Num, Show)

getLineCol :: Text -> Line -> Col -> Int -> (Line, Col)
getLineCol _ l c 0 = (l, c)
getLineCol t l c n = case uncons t of
  Just (x, t')
    | isNewline x -> getLineCol t' (l + 1) 0 (n - 1)
    | otherwise -> getLineCol t' l (c + 1) (n - 1)
  Nothing -> undefined

main :: IO ()
main = do
  source <- getArgs >>= readFile . head
  let printEither =
        either
          (print . getLineCol source (Line 1) (Col 1))
          print ::
          Either Int ([Stmt], Input) -> IO ()
  case parse program $ Input 0 source of
    Consumed x -> printEither x
    Empty x -> printEither x
