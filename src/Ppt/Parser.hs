module Ppt.Parser (parseFile, layoutFrame) where
import Text.ParserCombinators.Parsec (sepBy1, try, char, eof, many1,
                                      many, ParseError, digit,
                                      string, (<|>), (<?>), GenParser)
import Text.ParserCombinators.Parsec.Language (javaStyle)
import Text.ParserCombinators.Parsec.Prim (parse)
import qualified Text.ParserCombinators.Parsec.Token as P

import Ppt.BufferRep

lexer :: P.TokenParser ()
lexer = P.makeTokenParser
         (javaStyle {
            P.reservedNames = [ "emit", "frame", "double", "int", "float", "time", "default", "buffer" ]
          , P.caseSensitive = True
          } )

ws = P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
semi      = P.semi lexer
comma     = P.comma lexer
identifier= P.identifier lexer
resvd  = P.reserved lexer
reservedOp= P.reservedOp lexer
ch = char

type Parser t = GenParser Char () t

emitCmd :: Parser ELanguage
emitCmd = do { try (char 'C' >> char '+' >> char '+'); return ELangCpp }
          <|> do { string "C" >> return ELangC }
          <?> "language name"

emitType :: Parser ELanguage
emitType = do { resvd "emit"
              ; ws
              ; lang <- emitCmd
              ; ws
              ; return lang
              }

bufferValue = ( resvd "default" >> return Nothing )
              <|> do { ds <- many1 digit; ws; return (Just (read ds)) }

bufferCmd = do { resvd "buffer"
               ; name <- identifier
               ; size <- bufferValue
               ; return (EBuffer name size)
               }

primType = ( resvd "double" >> return PDouble )
           <|> ( resvd "int" >> return PInt )
           <|> ( resvd "float" >> return PFloat )
           <|> ( resvd "time" >> return PTime )
           <|> ( resvd "counter" >> return PCounter )
           <?> "type name"

element :: Bool -> Parser [FrameElement]
element mul = do { typ <- primType
                 ; names <- identifier `sepBy1` comma
                 ; semi
                 ; return (map (\n -> FMemberElem $ FMember  typ n mul) names)
                 }

-- |Primary member parser.  This will need some additions over time.
frameMember :: Parser [FrameElement]
frameMember = do { try (resvd "interval" >> element True) }
              <|> element False
              <?> "member declaration"

frame = do { resvd "frame"
           ; name <- identifier
           ; ch '{'
           ; members <- many frameMember
           ; ch '}'
           ; return (Frame name (concat members))
           }

parseFile = undefined
layoutFrame = undefined
