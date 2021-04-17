{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Ast
  ( Expr (..),
    Op (..),
    Stmt (..),
    bool,
    charLiteral,
    comment,
    expr,
    float,
    ident,
    int,
    manySpaces,
    space,
    stmt,
    stringLiteral,
  )
import Data.Semigroup (Min (..))
import Data.Text (Text)
import Parser (Consumed (..), Input (..), Parser (..), end, parse)
import System.Exit (exitFailure)
import Text.Printf (printf)

#define FILE_LINE __FILE__, __LINE__ + 1

eq :: (Eq a, Show a) => [(String, Int, a, a)] -> IO ()
eq [] = putChar '\n'
eq ((s, i, l, r) : xs)
  | l == r = do
    putChar '.'
    eq xs
  | otherwise = do
    putChar '\n'
    printf "%s:%d\n -> `%s`\n -> `%s`" s i (show l) (show r)
    exitFailure

parseWith :: Parser a -> Text -> Consumed a
parseWith p = parse (p <* end) . Input 0

main :: IO ()
main = do
  eq
    [ ( FILE_LINE,
        parseWith comment "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith comment "#\n",
        Consumed $ Right ((Min 1, ()), Input 2 "")
      ),
      ( FILE_LINE,
        parseWith comment " #\n",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith comment "# foo bar baz\n",
        Consumed $ Right ((Min 1, ()), Input 14 "")
      ),
      ( FILE_LINE,
        parseWith comment "# foo bar baz",
        Consumed $ Right ((Min 1, ()), Input 13 "")
      ),
      ( FILE_LINE,
        parseWith comment "# foo bar baz\n\n",
        Consumed $ Left 14
      ),
      ( FILE_LINE,
        parseWith comment "# foo bar baz\nx",
        Consumed $ Left 14
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith space "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith space "  \n\n\n  ",
        Consumed $ Right ((Min 1, ()), Input 7 "")
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith manySpaces "",
        Empty $ Right ((mempty, ()), Input 0 "")
      ),
      ( FILE_LINE,
        parseWith manySpaces "  \n# ...\n\n  # ??? \n\n ",
        Consumed $ Right ((Min 1, ()), Input 21 "")
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith int "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith int "1234",
        Consumed $ Right ((Min 1, 1234), Input 4 "")
      ),
      ( FILE_LINE,
        parseWith int "123x4",
        Consumed $ Left 3
      ),
      ( FILE_LINE,
        parseWith int " 1234",
        Empty $ Left 0
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith float "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith float "0.12345",
        Consumed $ Right ((Min 1, 0.12345), Input 7 "")
      ),
      ( FILE_LINE,
        parseWith float "1234.x56789",
        Consumed $ Left 5
      ),
      ( FILE_LINE,
        parseWith float " 1234.56789",
        Empty $ Left 0
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith bool "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith bool "true",
        Consumed $ Right ((Min 1, True), Input 4 "")
      ),
      ( FILE_LINE,
        parseWith bool "false",
        Consumed $ Right ((Min 1, False), Input 5 "")
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith ident "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith ident "xyz",
        Consumed $ Right ((Min 1, "xyz"), Input 3 "")
      ),
      ( FILE_LINE,
        parseWith ident "_xyz",
        Consumed $ Right ((Min 1, "_xyz"), Input 4 "")
      ),
      ( FILE_LINE,
        parseWith ident "_x_Y_Z_0",
        Consumed $ Right ((Min 1, "_x_Y_Z_0"), Input 8 "")
      ),
      ( FILE_LINE,
        parseWith ident "XYZ",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith ident "abc-def",
        Consumed $ Left 3
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith stringLiteral "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith stringLiteral "\"Hello, world!\"",
        Consumed $ Right ((Min 1, "Hello, world!"), Input 15 "")
      ),
      ( FILE_LINE,
        parseWith stringLiteral "\"\\\"Hello, world!\\\"\"",
        Consumed $ Right ((Min 1, "\"Hello, world!\""), Input 19 "")
      ),
      ( FILE_LINE,
        parseWith stringLiteral "\"\"",
        Consumed $ Right ((Min 1, ""), Input 2 "")
      ),
      ( FILE_LINE,
        parseWith stringLiteral "\"\n    \"",
        Consumed $ Right ((Min 1, "\n    "), Input 7 "")
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith charLiteral "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith charLiteral "'\0'",
        Consumed $ Right ((Min 2, '\0'), Input 3 "")
      ),
      ( FILE_LINE,
        parseWith charLiteral "''",
        Consumed $ Left 2
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith expr "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith expr "( )",
        Consumed $ Left 2
      ),
      ( FILE_LINE,
        parseWith expr "true",
        Consumed $ Right (EBool (Min 1, True), Input 4 "")
      ),
      ( FILE_LINE,
        parseWith expr "false",
        Consumed $ Right (EBool (Min 1, False), Input 5 "")
      ),
      ( FILE_LINE,
        parseWith expr "'\0'",
        Consumed $ Right (EChar (Min 2, '\0'), Input 3 "")
      ),
      ( FILE_LINE,
        parseWith expr "'\0\n'",
        Consumed $ Left 2
      ),
      ( FILE_LINE,
        parseWith expr "0.1",
        Consumed $ Right (EFloat (Min 1, 0.1), Input 3 "")
      ),
      ( FILE_LINE,
        parseWith expr "(0.1)",
        Consumed $ Right (EFloat (Min 2, 0.1), Input 5 "")
      ),
      ( FILE_LINE,
        parseWith expr "0.1.",
        Consumed $ Left 3
      ),
      ( FILE_LINE,
        parseWith expr "_foo_bar_0123",
        Consumed $ Right (EIdent (Min 1, "_foo_bar_0123"), Input 13 "")
      ),
      ( FILE_LINE,
        parseWith expr "(_foo_bar_0123)",
        Consumed $ Right (EIdent (Min 2, "_foo_bar_0123"), Input 15 "")
      ),
      ( FILE_LINE,
        parseWith expr "A_foo_bar_0123",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith expr "1234",
        Consumed $ Right (EInt (Min 1, 1234), Input 4 "")
      ),
      ( FILE_LINE,
        parseWith expr "(1234)",
        Consumed $ Right (EInt (Min 2, 1234), Input 6 "")
      ),
      ( FILE_LINE,
        parseWith expr "\"...\\\"?\\\"\"",
        Consumed $ Right (EStr (Min 1, "...\"?\""), Input 10 "")
      ),
      ( FILE_LINE,
        parseWith expr "(+ -a 2)",
        Consumed $
          Right
            ( EBinOp
                (Min 2, OpAdd)
                (EUnOp (Min 4, OpSub) (EIdent (Min 5, "a")))
                (EInt (Min 7, 2)),
              Input 8 ""
            )
      ),
      ( FILE_LINE,
        parseWith expr "(+ -a ?)",
        Consumed $ Left 1
      ),
      ( FILE_LINE,
        parseWith expr "(- 2 (- (+ 3 # ?\n2)))",
        Consumed $
          Right
            ( EBinOp
                (Min 2, OpSub)
                (EInt (Min 4, 2))
                ( EUnOp
                    (Min 7, OpSub)
                    ( EBinOp
                        (Min 10, OpAdd)
                        (EInt (Min 12, 3))
                        (EInt (Min 18, 2))
                    )
                ),
              Input 21 ""
            )
      ),
      ( FILE_LINE,
        parseWith expr "(- 2 (+ -3 + ?))",
        Consumed $ Left 5
      ),
      ( FILE_LINE,
        parseWith expr "( . a# \nb )",
        Consumed $
          Right
            ( EBinOp
                (Min 3, OpDot)
                (EIdent (Min 5, "a"))
                (EIdent (Min 9, "b")),
              Input 11 ""
            )
      ),
      ( FILE_LINE,
        parseWith expr "(. 1.0 - false)",
        Consumed $
          Right
            ( EBinOp
                (Min 2, OpDot)
                (EFloat (Min 4, 1.0))
                (EUnOp (Min 8, OpSub) (EBool (Min 10, False))),
              Input 15 ""
            )
      ),
      ( FILE_LINE,
        parseWith expr "(call f)",
        Consumed $ Right (ECall (EIdent (Min 7, "f")) [], Input 8 "")
      ),
      ( FILE_LINE,
        parseWith expr "(call f # ...\n)",
        Consumed $ Right (ECall (EIdent (Min 7, "f")) [], Input 15 "")
      ),
      ( FILE_LINE,
        parseWith expr "(call (. a f))",
        Consumed $
          Right
            ( ECall
                ( EBinOp
                    (Min 8, OpDot)
                    (EIdent (Min 10, "a"))
                    (EIdent (Min 12, "f"))
                )
                [],
              Input 14 ""
            )
      ),
      ( FILE_LINE,
        parseWith expr "( call f\n-2 a true )",
        Consumed $
          Right
            ( ECall
                (EIdent (Min 8, "f"))
                [ EUnOp (Min 10, OpSub) (EInt (Min 11, 2)),
                  EIdent (Min 13, "a"),
                  EBool (Min 15, True)
                ],
              Input 20 ""
            )
      ),
      ( FILE_LINE,
        parseWith expr "( call f\n-2  a#\ntrue )",
        Consumed $
          Right
            ( ECall
                (EIdent (Min 8, "f"))
                [ EUnOp (Min 10, OpSub) (EInt (Min 11, 2)),
                  EIdent (Min 14, "a"),
                  EBool (Min 17, True)
                ],
              Input 22 ""
            )
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith stmt "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith stmt "{}",
        Consumed $ Right (SBlock [], Input 2 "")
      ),
      ( FILE_LINE,
        parseWith stmt "x = true;",
        Consumed $
          Right
            ( SAssign (EIdent (Min 1, "x")) (EBool (Min 5, True)),
              Input 9 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "{#...\nx = true;\nx=false;\n}",
        Consumed $
          Right
            ( SBlock
                [ SAssign (EIdent (Min 7, "x")) (EBool (Min 11, True)),
                  SAssign (EIdent (Min 17, "x")) (EBool (Min 19, False))
                ],
              Input 26 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "x=true  # ...\n    ;",
        Consumed $
          Right
            ( SAssign (EIdent (Min 1, "x")) (EBool (Min 3, True)),
              Input 19 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "(call f);",
        Consumed $ Right (SEffect (ECall (EIdent (Min 7, "f")) []), Input 9 "")
      ),
      ( FILE_LINE,
        parseWith stmt "(\ncall f\n)\n;",
        Consumed $
          Right (SEffect (ECall (EIdent (Min 8, "f")) []), Input 12 "")
      ),
      ( FILE_LINE,
        parseWith stmt "if true {\n    (call f);\n}",
        Consumed $
          Right
            ( SIf
                (EBool (Min 4, True))
                [SEffect (ECall (EIdent (Min 21, "f")) [])],
              Input 25 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "iftrue{(call f)# ...\n;( call g ) # ...\n;}",
        Consumed $
          Right
            ( SIf
                (EBool (Min 3, True))
                [ SEffect (ECall (EIdent (Min 14, "f")) []),
                  SEffect (ECall (EIdent (Min 30, "g")) [])
                ],
              Input 41 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "if true { if false { } }",
        Consumed $
          Right
            ( SIf (EBool (Min 4, True)) [SIf (EBool (Min 14, False)) []],
              Input 24 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "if true { } else { }",
        Consumed $ Right (SIfElse (EBool (Min 4, True)) [] [], Input 20 "")
      ),
      ( FILE_LINE,
        parseWith stmt "if true { } else { if false { } else { } }",
        Consumed $
          Right
            ( SIfElse
                (EBool (Min 4, True))
                []
                [SIfElse (EBool (Min 23, False)) [] []],
              Input 42 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "if true { } else if false { } else { }",
        Consumed $
          Right
            ( SIfElse
                (EBool (Min 4, True))
                []
                [SIfElse (EBool (Min 21, False)) [] []],
              Input 38 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "if true { } else if false { }",
        Consumed $
          Right
            ( SIfElse
                (EBool (Min 4, True))
                []
                [SIf (EBool (Min 21, False)) []],
              Input 29 ""
            )
      )
    ]
