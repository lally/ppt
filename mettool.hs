import Text.ParserCombinators.Parsec (sepBy, try, char, eof, many, ParseError, string, (<|>), (<?>), GenParser)
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec.Token as P
import LLVM.Core as LC

--
-- Parser types
-- 
data FrameType = FDouble 
               | FFloat 
               | FInt
                 deriving Show

data FrameElement = FrameElement FrameType String
                    deriving Show

data FrameSpecification = FrameSpecification String [FrameElement]
                          deriving Show

data EmissionSpec = LangC 
                  | LangCpp
                    deriving Show
data FullSpecification = Spec EmissionSpec [FrameSpecification]
                         deriving Show


--emitType :: GenParser Char st EmissionSpec
--
-- Note: I'm not splitting out the whitespace.  Go a little higher level.
-- Then,  finally emit my struct decls & the 

lexer :: TokenParser ()
lexer = makeTokenParser 
         (javaStyle {
            reservedNames = [ "emit", "frame", "double", "int", "float" ]
          , caseSensitive = True
          } )

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer
                           

emitCmd = do { try (char 'C' >> char '+' >> char '+'); return LangCpp }
          <|> do { string "C" >> return LangC }
          <?> "language name"

ch c = do { Main.whiteSpace; x <- char c; Main.whiteSpace; return x }

emitType = do { Main.reserved "emit" 
              ; Main.whiteSpace
              ; lang <- emitCmd
              ; Main.whiteSpace
              ; return lang
              }

elementType = ( Main.reserved "double" >> return FDouble )
              <|> ( Main.reserved "int" >> return FInt )
              <|> ( Main.reserved "float" >> return FFloat )
              <?> "type name"

--element :: Parser FrameElement
element = do { typ <- elementType
             ; name <- Main.identifier
             ; Main.semi
             ; return (FrameElement typ name)
             }

--frameSpec :: Parser FrameSpecification
frameSpec = do { Main.reserved "frame"
               ; name <- Main.identifier
               ; ch '{'
               ; elem <- many element
               ; ch '}'
               ; return (FrameSpecification name elem)
               }

type Parser t = GenParser Char () t

frameSpecs :: Parser [FrameSpecification]
frameSpecs = do { fr <- many (frameSpec)
                ; return fr
                }


commandFile :: GenParser Char () FullSpecification
commandFile =
            do { emitType <- emitType
               ; frames <- frameSpecs
               ; return (Spec emitType frames)
               }
               

testParse :: Show a => Parser a -> String -> Either ParseError a
testParse p input = parse (do { Main.whiteSpace
                            ; x <- p
                            ; eof
                            ; return x
                            }) "input" input


main :: IO ()
main = do { LC.initializeNativeTarget }

