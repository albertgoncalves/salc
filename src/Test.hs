{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Ast
  ( Expr (..),
    Op (..),
    Stmt (..),
    Type (..),
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
    type',
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
        parseWith charLiteral "'_'",
        Consumed $ Right ((Min 2, '_'), Input 3 "")
      ),
      ( FILE_LINE,
        parseWith charLiteral "''",
        Consumed $ Left 2
      ),
      ( FILE_LINE,
        parseWith charLiteral "'\\''",
        Consumed $ Right ((Min 2, '\''), Input 4 "")
      ),
      ( FILE_LINE,
        parseWith charLiteral "'\\n'",
        Consumed $ Right ((Min 2, '\n'), Input 4 "")
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
        parseWith expr "($ f)",
        Consumed $ Right (ECall (EIdent (Min 4, "f")) [], Input 5 "")
      ),
      ( FILE_LINE,
        parseWith expr "($ f # ...\n)",
        Consumed $ Right (ECall (EIdent (Min 4, "f")) [], Input 12 "")
      ),
      ( FILE_LINE,
        parseWith expr "($ (. a f))",
        Consumed $
          Right
            ( ECall
                ( EBinOp
                    (Min 5, OpDot)
                    (EIdent (Min 7, "a"))
                    (EIdent (Min 9, "f"))
                )
                [],
              Input 11 ""
            )
      ),
      ( FILE_LINE,
        parseWith expr "( $ f\n-2 a true )",
        Consumed $
          Right
            ( ECall
                (EIdent (Min 5, "f"))
                [ EUnOp (Min 7, OpSub) (EInt (Min 8, 2)),
                  EIdent (Min 10, "a"),
                  EBool (Min 12, True)
                ],
              Input 17 ""
            )
      ),
      ( FILE_LINE,
        parseWith expr "( $ f\n-2  a#\ntrue )",
        Consumed $
          Right
            ( ECall
                (EIdent (Min 5, "f"))
                [ EUnOp (Min 7, OpSub) (EInt (Min 8, 2)),
                  EIdent (Min 11, "a"),
                  EBool (Min 14, True)
                ],
              Input 19 ""
            )
      ),
      ( FILE_LINE,
        parseWith expr "(< a b)",
        Consumed $
          Right
            ( EBinOp
                (Min 2, OpLT)
                (EIdent (Min 4, "a"))
                (EIdent (Min 6, "b")),
              Input 7 ""
            )
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith type' "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith type' "i32",
        Consumed $ Right ((Min 1, TI32), Input 3 "")
      ),
      ( FILE_LINE,
        parseWith type' "f32",
        Consumed $ Right ((Min 1, TF32), Input 3 "")
      ),
      ( FILE_LINE,
        parseWith type' "bool",
        Consumed $ Right ((Min 1, TBool), Input 4 "")
      ),
      ( FILE_LINE,
        parseWith type' "char",
        Consumed $ Right ((Min 1, TChar), Input 4 "")
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
        parseWith stmt "($f);",
        Consumed $ Right (SEffect (ECall (EIdent (Min 3, "f")) []), Input 5 "")
      ),
      ( FILE_LINE,
        parseWith stmt "(\n$ f\n)\n;",
        Consumed $ Right (SEffect (ECall (EIdent (Min 5, "f")) []), Input 9 "")
      ),
      ( FILE_LINE,
        parseWith stmt "if true {\n    ($ f);\n}",
        Consumed $
          Right
            ( SIf
                (EBool (Min 4, True))
                [SEffect (ECall (EIdent (Min 18, "f")) [])],
              Input 22 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "iftrue{($ f)# ...\n;( $ g ) # ...\n;}",
        Consumed $
          Right
            ( SIf
                (EBool (Min 3, True))
                [ SEffect (ECall (EIdent (Min 11, "f")) []),
                  SEffect (ECall (EIdent (Min 24, "g")) [])
                ],
              Input 35 ""
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
      ),
      ( FILE_LINE,
        parseWith stmt "i32 x;",
        Consumed $
          Right (SDecl (Min 1, TI32) (EIdent (Min 5, "x")) Nothing, Input 6 "")
      ),
      ( FILE_LINE,
        parseWith stmt "i32 x = 0;",
        Consumed $
          Right
            ( SDecl
                (Min 1, TI32)
                (EIdent (Min 5, "x"))
                (Just $ EInt (Min 9, 0)),
              Input 10 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "i32 x = (- 1.0 2.0);",
        Consumed $
          Right
            ( SDecl
                (Min 1, TI32)
                (EIdent (Min 5, "x"))
                ( Just $
                    EBinOp
                      (Min 10, OpSub)
                      (EFloat (Min 12, 1.0))
                      (EFloat (Min 16, 2.0))
                ),
              Input 20 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "loop { }",
        Consumed $ Right (SLoop [], Input 8 "")
      ),
      ( FILE_LINE,
        parseWith stmt "loop { i32 x; x = 0; }",
        Consumed $
          Right
            ( SLoop
                [ SDecl (Min 8, TI32) (EIdent (Min 12, "x")) Nothing,
                  SAssign (EIdent (Min 15, "x")) (EInt (Min 19, 0))
                ],
              Input 22 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "break;",
        Consumed $ Right (SBreak (Min 1, ()), Input 6 "")
      ),
      ( FILE_LINE,
        parseWith stmt "continue\n    ;",
        Consumed $ Right (SCont (Min 1, ()), Input 14 "")
      ),
      ( FILE_LINE,
        parseWith stmt "return\ntrue\n;",
        Consumed $ Right (SRet (EBool (Min 8, True)), Input 13 "")
      )
    ]
