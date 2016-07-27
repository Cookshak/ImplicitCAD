module Graphics.Implicit.ExtOpenScad.Parser.Lexer where

import Control.Monad.Identity;
import Text.Parsec.Token
import Text.ParserCombinators.Parsec  hiding (State)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

-- The token parsers are in roughly the same order as the OpenSCAD lexer.l Flex source, to make it easier to compare them.

openScadStyle :: GenLanguageDef String u0 Identity
openScadStyle
    = emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , nestedComments = True
    , identStart = letter <|> char '$' <|> char '_'
    , identLetter = alphaNum <|> char '_'
    , reservedNames = ["module", "function", "if", "else", "let", "for", "each", "true", "false", "undef", "include", "use"]
    , reservedOpNames= ["<=", ">=", "==", "!=", "&&", "||"]
    , caseSensitive = True
    }

lexer :: GenTokenParser String st Identity
lexer = makeTokenParser openScadStyle

-- Deal with unicode later.

whiteSpace :: GenParser Char st ()
whiteSpace = P.whiteSpace lexer
matchModule :: GenParser Char st ()
matchModule = P.reserved lexer "module"
matchFunction :: GenParser Char st ()
matchFunction = P.reserved lexer "function"
matchIf :: GenParser Char st ()
matchIf = P.reserved lexer "if"
matchElse :: GenParser Char st ()
matchElse = P.reserved lexer "else"
matchLet :: GenParser Char st ()
matchLet = P.reserved lexer "let"
matchFor :: GenParser Char st ()
matchFor = P.reserved lexer "for"
matchEach :: GenParser Char st ()
matchEach = P.reserved lexer "each"
matchTrue :: GenParser Char st ()
matchTrue = P.reserved lexer "true"
matchFalse :: GenParser Char st ()
matchFalse = P.reserved lexer "false"
matchUndef :: GenParser Char st ()
matchUndef = P.reserved lexer "undef"
matchInclude :: GenParser Char st ()
matchInclude = P.reserved lexer "include"
matchUse :: GenParser Char st ()
matchUse = P.reserved lexer "use"

number :: GenParser Char st (Either Integer Double)
number =
    do
        _ <- char '.' <?> "" -- OpenSCAD supports floating point numbers that start with a decimal.
        fractional <- many1 digit
        expont <-
            do
                _ <- oneOf "eE"
                sign <- (oneOf "-+" <?> "exponent") <|> return '+'
                expo <- many1 digit <?> "exponent"
                return $ 'e':sign:expo
            <|>
                return ""
        return $ Right ((read $ "0." ++ fractional ++ expont) :: Double)
    <|>
        P.naturalOrFloat lexer

identifier :: GenParser Char st String
identifier = P.identifier lexer

literalString :: GenParser Char st String
literalString = P.stringLiteral lexer -- not sure if the Parsec idea of string literal matches OpenSCAD. Consider unicode, \x hex codes

-- single line comments, multiline comments and whitespace are consumed by the other token types, since the parser doesn't really need them to build the AST

matchLE :: GenParser Char st String
matchLE = P.reservedOp lexer "<=" >> return "<="
matchGE :: GenParser Char st String
matchGE = P.reservedOp lexer ">=" >> return ">="
matchEQ :: GenParser Char st String
matchEQ = P.reservedOp lexer "==" >> return "=="
matchNE :: GenParser Char st String
matchNE = P.reservedOp lexer "!=" >> return "!="
matchAND :: GenParser Char st String
matchAND = P.reservedOp lexer "&&" >> return "&&"
matchOR :: GenParser Char st String
matchOR = P.reservedOp lexer "||" >> return "||"
matchCAT :: GenParser Char st String
matchCAT = P.reservedOp lexer "++" >> return "++"

-- single character tokens can be handled fine in the main parser, just strip the trailing whitespace.
matchTok :: Char -> GenParser Char st String
matchTok tok = P.lexeme lexer $ P.symbol lexer [tok]
