module Ppt.Parser (parseFile, compileFrame) where
import Text.ParserCombinators.Parsec (sepBy1, try, char, eof, many1, alphaNum,
                                      many, ParseError, digit, noneOf,
                                      string, (<|>), (<?>), GenParser)
import Text.ParserCombinators.Parsec.Language (javaStyle)
import Text.ParserCombinators.Parsec.Prim (parse)
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.List
import Control.Monad
import Control.Monad.Trans.Either
import Data.Aeson (encode, decode)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Ppt.BufferRep

lexer :: P.TokenParser ()
lexer = P.makeTokenParser
         (javaStyle {
            P.reservedNames = [ "emit", "frame", "double", "int", "float",
                                "time", "default", "buffer" ]
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

primType = ( resvd "double" >> return PDouble )
           <|> ( resvd "int" >> return PInt )
           <|> ( resvd "float" >> return PFloat )
           <|> ( resvd "time" >> return PTime )
           <|> ( resvd "counter" >> return PCounter )
           <?> "type name"
-- TODO: replace primType here with a higher-level production that
-- also takes interval and other keywords.  They form a list of
-- qualified-type-specifiers, which get processed by another function
-- into the FMemberElem.
element :: Bool -> Parser [FrameElement]
element mul = do { typ <- primType
                 ; names <- identifier `sepBy1` comma
                 ; semi
                 ; return (map (\n -> FMemberElem $ FMember typ n mul) names)
                 }

-- |Primary member parser.  This will need some additions over time.
-- The syntax will be:
-- - interval time duration;
-- - int count;
-- - [live] [exported] stat field = gap(duration) / count
frameMember :: Parser [FrameElement]
frameMember = do { try (resvd "interval" >> element True) }
              <|> element False
              <?> "member declaration"

frame = do { resvd "frame"
           ; name <- identifier
           ; ch '{'
           ; ws
           ; members <- many frameMember
           ; ws
           ; ch '}'
           ; return (Frame name (concat members))
           }


emitCmd :: Parser [ELanguage]
emitCmd = do { try (char 'C' >> char '+' >> char '+'); return [ELangCpp] }
          <|> do { string "C" >> return [ELangC] }
          <?> "language name"

emitType :: Parser [ELanguage]
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
               ; return [EBuffer name size]
               }

timeSpecOption :: Parser ETimeSource
timeSpecOption = (resvd "realtime" >> return ETimeClockRealtime)
                 <|> (resvd "realtime_coarse" >> return ETimeClockRealtimeCoarse)
                 <|> (resvd "monotonic" >> return ETimeClockMonotonic)
                 <|> (resvd "monotonic_course" >> return ETimeClockMonotonicCoarse)
                 <|> (resvd "monotonic_raw" >> return ETimeClockMonotonicRaw)
                 <|> (resvd "boottime" >> return ETimeClockBoottime)
                 <|> (resvd "proc_cputime" >> return ETimeClockProcessCputimeId)
                 <|> (resvd "thread_cputime" >> return ETimeClockThreadCputimeId)
                 <?> "clock type"

timeOption ::Parser ETimeRep
timeOption = (resvd "timeval" >> return ETimeVal)
             <|> do { resvd "timespec"
                    ; kind <- timeSpecOption
                    ; return (ETimeSpec kind)
                    }
             <?> "time type"

boolOption :: Parser Bool
boolOption = (resvd "true" >> return True)
             <|> (resvd "True" >> return True)
             <|> (resvd "false" >> return False)
             <|> (resvd "False" >> return False)
             <?> "bool"

runtimeOption :: Parser ERuntime
runtimeOption = do { resvd "multithread"
                   ; val <- boolOption
                   ; return (ERuntime val) }

possiblyQuotedString :: Parser String
possiblyQuotedString = quotedString <|> many (char '_' <|> alphaNum)

quotedString = do
    char '"'
    x <- many (noneOf "\"" <|> (char '\\' >> char '\"'))
    char '"'
    return x

tagOption = do { resvd "tag"
               ; key <- possiblyQuotedString
               ; val <- possiblyQuotedString
               ; return (Tag key val) }

data PartialOption = OptLang ELanguage
                   | OptBuffer EBuffer
                   | OptTime ETimeRep
                   | OptRuntime ERuntime
                   | OptTags [ETag]
                   deriving (Eq, Show)

optionParser :: Parser [PartialOption]
optionParser = (do { resvd "time"
                   ; timeOpt <- timeOption
                   ; return [OptTime timeOpt] })
               <|> (do { resvd "runtime"
                       ; run <- runtimeOption
                       ; return [OptRuntime run] })
               <|> (do { tag <- tagOption
                       ; return [OptTags [tag]] })
               <?> "option type: time, runtime, tag"

optionSplitter :: PartialOption -> ([ELanguage], [EBuffer], [ETimeRep], [ERuntime], [[ETag]])
optionSplitter (OptLang l)    = ([l], [], [], [], [])
optionSplitter (OptBuffer b)  = ([], [b], [], [], [])
optionSplitter (OptTime t)    = ([], [], [t], [], [])
optionSplitter (OptRuntime r) = ([], [], [], [r], [])
optionSplitter (OptTags ts)   = ([], [], [], [], [ts])

defaultEmit = Emit (EBuffer "unlisted" Nothing) ELangCpp (ETimeSpec ETimeClockMonotonic) (ERuntime False) []

optionCombine :: [PartialOption] -> Either String EmitOptions
optionCombine opts = let
  addUp (a, b, c, d, e) (f, g, h, i, j) = (a ++ f, b ++ g, c ++ h, d ++ i, e ++ j)
  (langs, buffers, timereps, runtimes, rawTags) = foldl' addUp ([], [], [], [], []) (
    map optionSplitter opts)
  tags = concat rawTags
  min1 :: [a] -> String -> Either String a
  min1 (x:_)  _ = Right x
  min1 [] name = Left (name ++ " required")
  max1 :: [a] -> String -> a -> Either String a
  max1 (x:[]) _ _ = Right x
  max1 [] _ def = Right def
  max1 _ name def = Left ("Only 1 " ++ name ++ " allowed")
  exact1 :: [a] -> String -> Either String a
  exact1 (x:[]) _  = Right x
  exact1 _ name = Left ("Exactly 1 " ++ name ++ " required")
  in (Emit <$> max1 buffers "buffer line" (EBuffer "unlisted" Nothing)
       <*> exact1 langs "Language"
       <*> max1 timereps "time representation" (ETimeSpec ETimeClockMonotonic)
       <*> max1 runtimes "multithread option" (ERuntime False)
       <*> (pure tags))

headParser :: Parser EmitOptions
headParser = do { options <- many ((resvd "option" >> optionParser)
                                   <|> (do { r <- emitCmd ; return (map OptLang r)})
                                   <|> (do { r <- bufferCmd; return (map OptBuffer r)})
                                   <?> "header matter")
                ; case (optionCombine $ concat options) of
                    Left msg -> fail msg
                    Right opts -> return opts
                }


parseText :: Show a => Parser a -> String -> String -> Either ParseError a
parseText p input fname = parse (do { ws
                                    ; x <- p
                                    ; eof
                                    ; return x
                                    }) fname input
parseFile :: String -> String -> Either ParseError Buffer
parseFile input fname = parse (do { ws
                                  ; head <- headParser
                                  ; frames <- many frame
                                  ; return (Buffer head frames) }) fname input

readBuffer :: String -> Maybe Buffer
readBuffer s = decode (pack s)

saveBuffer :: Buffer -> String
saveBuffer b = unpack $ encode b


compileFrame = undefined

