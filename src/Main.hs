{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Ast (Stmt, isNewline, program)
import Data.Text (Text, foldl', take)
import Data.Text.IO (readFile)
import Parser (Consumed (..), Input (..), parse)
import System.Environment (getArgs)
import Prelude hiding (readFile, take)

newtype Line = Line Int
  deriving (Num, Show)

newtype Col = Col Int
  deriving (Num, Show)

getLineCol :: Text -> Int -> (Line, Col)
getLineCol t n = foldl' f (Line 1, Col 1) $ take n t
  where
    f (l, c) x
      | isNewline x = (l + 1, 1)
      | otherwise = (l, c + 1)

main :: IO ()
main = do
  source <- getArgs >>= readFile . head
  let f =
        either (print . getLineCol source) print ::
          Either Int ([Stmt], Input) -> IO ()
  case parse program $ Input 0 source of
    Consumed x -> f x
    Empty x -> f x
