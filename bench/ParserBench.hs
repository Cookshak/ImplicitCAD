import Criterion.Main
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Expr (origExpr)
import Graphics.Implicit.ExtOpenScad.Parser.Statement (origParseProgram)
import Graphics.Implicit.ExtOpenScad.Parser.AltExpr (altExpr)
import Graphics.Implicit.ExtOpenScad.Parser.AltStatement (altParseProgram)
import Text.ParserCombinators.Parsec hiding (State)
import Text.Printf

lineComment :: Int -> String
lineComment width = "//" ++ ['x' | _ <- [1..width]] ++ "\n"

lineComments :: Int -> String
lineComments n = concat [lineComment 80 | _ <- [1..n]]
                 ++ assignments 1 -- to avoid empty file

blockComment :: Int -> Int -> String
blockComment lineCount width =
  "/*" ++ concat [['x' | _ <- [1..width]] ++ "\n" | _ <- [1..lineCount]] ++ "*/"

blockComments :: Int -> Int -> String
blockComments lineCount n = concat [blockComment lineCount 40 | _ <- [1..n]]
                            ++ assignments 1 -- to avoid empty file

assignments :: Int -> String
assignments n = concat ["x = (foo + bar);\n" | _ <- [1..n]]

intList :: Int -> String
intList n = "[" ++ concat [(show i) ++ "," | i <- [1..n]] ++ "0]"

parseExpr :: (GenParser Char () Expr) -> String -> Expr
parseExpr exprParser s = case parse exprParser "src" s of
               Left err -> error (show err)
               Right e -> e

parseStatements :: (SourceName -> [Char] -> Either ParseError [StatementI]) -> String -> [StatementI]
parseStatements parseProgram s = case parseProgram "src" s of
                     Left err -> error (show err)
                     Right e -> e

deepArithmetic :: Int -> String
deepArithmetic n
  | n == 0 = "1"
  | otherwise = printf "%s + %s * (%s - %s)" d d d d
                where
                  d = deepArithmetic (n - 1)

run :: String -> (String -> a) -> String -> Benchmark
run name func input =
  env (return $ input) $ \s ->
  bench name $ whnf func s

main :: IO ()
main =
  defaultMain $
  [ bgroup "orig"
    [ bgroup "comments"
      [ run "line" (parseStatements origParseProgram) (lineComments 5000)
      , run "block" (parseStatements origParseProgram) (blockComments 10 500)
      ]
    , run "assignments" (parseStatements origParseProgram) (assignments 100)
    , run "int list" (parseExpr origExpr) (intList 1000)
    , run "deep arithmetic" (parseExpr origExpr) (deepArithmetic 3)
    ]
  , bgroup "alt"
    [ bgroup "comments"
      [ run "line" (parseStatements altParseProgram) (lineComments 5000)
      , run "block" (parseStatements altParseProgram) (blockComments 10 500)
      ]
    , run "assignments" (parseStatements altParseProgram) (assignments 100)
    , run "int list" (parseExpr altExpr) (intList 1000)
    , run "deep arithmetic" (parseExpr altExpr) (deepArithmetic 3)
    ]
  ]
