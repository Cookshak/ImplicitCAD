module ParserSpec.Expr (exprSpec) where

import Test.Hspec
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Expr
import Graphics.Implicit.ExtOpenScad.Parser.Statement
import ParserSpec.Util
import Text.ParserCombinators.Parsec hiding (State)
import Data.Either

infixr 1 -->
(-->) :: String -> Expr -> Expectation
(-->) source expr =
  (parseExpr source) `shouldBe` Right expr

infixr 1 -->+
(-->+) :: String -> (Expr, String) -> Expectation
(-->+) source (result, leftover) =
  (parseWithLeftOver expr0 source) `shouldBe` (Right (result, leftover))

isUndef source = 
    let result = (parseExpr source)
        undef (Right (LitE OUndefined)) = True
        undef _ = False
    in
        undef result `shouldBe` True
 
ternaryIssue :: Expectation -> Expectation
ternaryIssue _ = pendingWith "parser doesn't handle ternary operator correctly"

listIssue :: Expectation -> Expectation
listIssue _ = pendingWith "the list construct does not exist in OpenSCAD and provides no syntactic or semantic advantage, and may make the parser more complex."

enableAlternateParser = True

originalParserAdditionAstStyle :: Expectation -> Expectation
originalParserAdditionAstStyle a = 
    if enableAlternateParser 
    then pendingWith "original parser generates + expression trees differently than - expression trees. The experimental parser treats them the same."
    else a
    
experimentalParserAstStyle a = 
    if enableAlternateParser 
    then a 
    else pendingWith "The test was written for the experimental parser's AST generation."

experimentalFeature a = 
    if enableAlternateParser 
    then a 
    else pendingWith "This tests a feature of the experimental parser that does not work in the original parser."

logicalSpec :: Spec
logicalSpec = do
  it "handles not" $ "!foo" --> (app' "!" [Var "foo"])
  it "handles and/or" $ do
    "foo && bar" --> app' "&&" [Var "foo", Var "bar"]
    "foo || bar" --> app' "||" [Var "foo", Var "bar"]
  describe "ternary operator" $ do
    specify "with primitive expressions" $
      "x ? 2 : 3" --> app' "?" [Var "x", num 2, num 3]
    specify "with parenthesized comparison" $
      "(1 > 0) ? 5 : -5" --> app' "?" [app' ">" [num 1, num 0], num 5, num (-5)]
    specify "with comparison in head position" $
      "1 > 0 ? 5 : -5" --> app' "?" [app' ">" [num 1, num 0], num 5, num (-5)]
    specify "with comparison in head position, and addition in tail" $
     originalParserAdditionAstStyle $ ternaryIssue $
      "1 > 0 ? 5 : 1 + 2" -->
      app' "?" [app' ">" [num 1, num 0], num 5, app "+" [num 1, num 2]]
    specify "with comparison in head position, and addition in tail" $
     experimentalFeature $
      "1 > 0 ? 5 : 1 + 2" -->
      app' "?" [app' ">" [num 1, num 0], num 5, app' "+" [num 1, num 2]]
    specify "nested in true and false expressions" $
     experimentalFeature $
      "c0 ? c1 ? t1 : f1 : c2 ? t2 : f2" -->
      app' "?" [Var "c0", app' "?" [Var "c1",Var "t1",Var "f1"], app' "?" [Var "c2",Var "t2",Var "f2"]]

literalSpec :: Spec
literalSpec = do
  describe "integers" $ do
    it "handles integers" $ do
      "12356" -->  num 12356
    it "handles positive leading zero integers" $ do
      "000012356" -->  num 12356
    it "handles zero integer" $ do
      "0" -->  num 0
    it "handles leading zero integer" $ do
      "0000" -->  num 0
  it "handles floats" $ do
    "23.42" -->  num 23.42
  describe "booleans" $ do
    it "accepts true" $ "true" --> bool True
    it "accepts false" $ "false" --> bool False

letBindingSpec :: Spec
letBindingSpec = do
    it "handles let with integer binding and spaces" $ do
        "let ( a = 1 ) a" --> lambda' [Name "a"] (Var "a") [num 1]
    it "handles multiple variable let" $ do
        "let (a = x, b = y) a + b" --> lambda' [Name "a"] ((lambda' [Name "b"] (app' "+" [Var "a", Var "b"])) [Var "y"]) [Var "x"]
    it "handles empty let" $ do
        "let () a" --> (Var "a")
    it "handles nested let" $ do
        "let(a=x) let(b = y) a + b" --> lambda' [Name "a"] ((lambda' [Name "b"] (app' "+" [Var "a", Var "b"])) [Var "y"]) [Var "x"]
    it "handles let on right side of a binary operator" $ do
        "1 + let(b = y) b" --> lambda' [Name "a"] ((lambda' [Name "b"] (app' "+" [Var "a", Var "b"])) [Var "y"]) [Var "x"]
    
exprSpec :: Spec
exprSpec = do
  describe "literals" literalSpec
  describe "identifiers" $ do
    it "accepts valid variable names" $ do
      "foo" --> Var "foo"
      "foo_bar" --> Var "foo_bar"
  describe "grouping" $ do
    it "allows parens" $ do
      "( false )" -->  bool False
    it "handles empty vectors" $ do
      "[]" -->  ListE []
    it "handles single element vectors" $ do
      "[a]" -->  ListE [Var "a"]
    it "handles vectors" $ do
      "[ 1, 2, 3 ]" -->  ListE [num 1, num 2, num 3]
    it "handles nested vectors" $ do
      "[ 1, [2, 7], [3, 4, 5, 6] ]" -->  ListE [num 1, ListE [num 2, num 7], ListE [num 3, num 4, num 5, num 6]]
    it "handles lists" $ do
     listIssue $ 
      "( 1, 2, 3 )" -->  ListE [num 1, num 2, num 3]
    it "handles generators" $
      originalParserAdditionAstStyle $ 
      "[ a : 1 : b + 10 ]" -->
      (app "list_gen" [Var "a", num 1, app "+" [Var "b", num 10]])
    it "handles generators" $
     experimentalParserAstStyle $
      "[ a : b ]" -->
      (app "list_gen" [Var "a", num 1, Var "b"])
    it "handles generators with expression" $
     experimentalParserAstStyle $
      "[ a : b + 10 ]" -->
      (app "list_gen" [Var "a", num 1, app' "+" [Var "b", num 10]])
    it "handles increment generators" $
     experimentalParserAstStyle $
      "[ a : 3 : b + 10 ]" -->
      (app "list_gen" [Var "a", num 3, app' "+" [Var "b", num 10]])
    it "handles indexing" $
      "foo[23]" --> Var "index" :$ [Var "foo", num 23]
    it "handles multiple indexes" $
      "foo[23][12]" --> Var "index" :$ [Var "index" :$ [Var "foo", num 23], num 12]
    it "handles single function call with single argument" $
      "foo(1)" --> Var "foo" :$ [num 1]
    it "handles single function call with multiple arguments" $
      "foo(1, 2, 3)" --> Var "foo" :$ [num 1, num 2, num 3]
    it "handles multiple function calls" $
      "foo(1)(2)(3)" --> ((Var "foo" :$ [num 1]) :$ [num 2]) :$ [num 3]

  describe "arithmetic" $ do
    it "handles unary -" $ do
      "-42" --> num (-42)
    it "handles unary +" $ do
      "+42" -->  num 42
    it "handles unary - with extra spaces" $ do
      "-  42" --> num (-42)
    it "handles unary + with extra spaces" $ do
      "+  42" -->  num 42
    it "handles unary - with parentheses" $ do
      "-(4 - 3)" --> app' "-" [app' "-" [num 4, num 3]]
    it "handles unary + with parentheses" $ do
      "+(4 - 1)" -->  app' "-" [num 4, num 1]
    it "handles unary - with identifier" $ do
      "-foo" --> app' "-" [Var "foo"]
    it "handles unary + with identifier" $ do
      "+foo" -->  Var "foo"
    it "handles unary - with string literal" $ do
      isUndef "-\"foo\""
    it "handles unary + with string literal" $ do
      "+\"foo\"" -->  stringLiteral "foo"
    it "handles +" $ do
     originalParserAdditionAstStyle $ 
      "1 + 2" --> app "+" [num 1, num 2]
     originalParserAdditionAstStyle $ 
      "1 + 2 + 3" --> app "+" [num 1, num 2, num 3]
    it "handles 2 term +" $ do
     experimentalParserAstStyle $
      "1 + 2" --> app' "+" [num 1, num 2]
    it "handles > 2 term +" $ do
     experimentalParserAstStyle $
      "1 + 2 + 3" --> app' "+" [app' "+" [num 1, num 2], num 3]
    it "handles -" $ do
      "1 - 2" --> app' "-" [num 1, num 2]
      "1 - 2 - 3" --> app' "-" [app' "-" [num 1, num 2], num 3]
    it "handles +/- in combination" $ do
     originalParserAdditionAstStyle $
      "1 + 2 - 3" --> app "+" [num 1, app' "-" [num 2, num 3]]
     originalParserAdditionAstStyle $
      "2 - 3 + 4" --> app "+" [app' "-" [num 2, num 3], num 4]
     originalParserAdditionAstStyle $
      "1 + 2 - 3 + 4" --> app "+" [num 1, app' "-" [num 2, num 3], num 4]
     originalParserAdditionAstStyle $
      "1 + 2 - 3 + 4 - 5 - 6" --> app "+" [num 1,
                                           app' "-" [num 2, num 3],
                                           app' "-" [app' "-" [num 4, num 5],
                                                     num 6]]
    describe "handles +/- in combination, deeper tree AST" $ do
      it "handles 1 + 2 - 3" $ do
       experimentalParserAstStyle $
        "1 + 2 - 3" --> app' "-" [app' "+" [num 1, num 2], num 3]
      it "handles 2 - 3 + 4" $ do
       experimentalParserAstStyle $
        "2 - 3 + 4" --> app' "+" [app' "-" [num 2, num 3], num 4]
      it "handles 1 + 2 - 3 + 4" $ do
       experimentalParserAstStyle $
        "1 + 2 - 3 + 4" --> app' "+" [app' "-" [app' "+" [num 1, num 2], num 3], num 4]
      it "handles 1 + 2 - 3 + 4 - 5 - 6" $ do
       experimentalParserAstStyle $
        "1 + 2 - 3 + 4 - 5 - 6" --> app' "-" [app' "-" [app' "+" [app' "-" [app' "+" [num 1, num 2], num 3], num 4], num 5], num 6]
    it "handles exponentiation" $
      "x ^ y" -->  app' "^" [Var "x", Var "y"]
    it "handles multiple exponentiations" $
      "x ^ y ^ z" -->  app' "^" [app' "^" [Var "x", Var "y"], Var "z"]
    it "handles *" $ do
     originalParserAdditionAstStyle $
      "3 * 4" -->  app "*" [num 3, num 4]
     originalParserAdditionAstStyle $
      "3 * 4 * 5" -->  app "*" [num 3, num 4, num 5]
    it "handles *" $ do
     experimentalParserAstStyle $
      "3 * 4" -->  app' "*" [num 3, num 4]
     experimentalParserAstStyle $
      "3 * 4 * 5" -->  app' "*" [app' "*" [num 3, num 4], num 5]
    it "handles /" $
      "4.2 / 2.3" -->  app' "/" [num 4.2, num 2.3]
    it "handles precedence" $
     originalParserAdditionAstStyle $
      parseExpr "1 + 2 / 3 * 5" `shouldBe`
       (Right $ app "+" [num 1, app "*" [app' "/" [num 2, num 3], num 5]])
    it "handles precedence" $
     experimentalParserAstStyle $
      parseExpr "1 + 2 / 3 * 5" `shouldBe`
      (Right $ app' "+" [num 1, app' "*" [app' "/" [num 2, num 3], num 5]])
  it "handles append" $
   originalParserAdditionAstStyle $
    parseExpr "foo ++ bar ++ baz" `shouldBe`
    (Right $ app "++" [Var "foo", Var "bar", Var "baz"])
  it "handles append" $
   experimentalParserAstStyle $
    parseExpr "foo ++ bar ++ baz" `shouldBe`
    (Right $ app' "++" [app' "++" [Var "foo", Var "bar"], Var "baz"])
  describe "logical operators" logicalSpec
  describe "let expressions" letBindingSpec
  