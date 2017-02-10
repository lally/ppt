module Ppt.Frame.Parser where -- (parseFile, compileFrame) where 
import Text.ParserCombinators.Parsec (sepBy1, try, char, eof, many1, alphaNum,
                                      many, ParseError, digit, noneOf,
                                      string, (<|>), (<?>), GenParser)
import Text.ParserCombinators.Parsec.Language (javaStyle)
import Text.ParserCombinators.Parsec.Prim (parse)
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.List
import Control.Exception (handle)
import Control.Monad
import Control.Monad.Trans.Either
import Data.Aeson (encode, decode)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Ppt.Frame.ParsedRep

lexer :: P.TokenParser ()
lexer = P.makeTokenParser
         (javaStyle {
            P.reservedNames = [
                "emit", "frame", "double", "int", "float", "option",
                  "time", "default", "buffer", "counter", "interval",
                  "calc", "gap", "var", "mean" ]
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


emitType :: Parser ELanguage
emitType = do { try (char 'C' >> char '+' >> char '+'); return ELangCpp }
           <|> do { string "C" >> return ELangC }
           <?> "language name"

emitCmd :: Parser ELanguage
emitCmd = do { resvd "emit"
              ; ws
              ; lang <- emitType
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
               ; ws
               ; val <- possiblyQuotedString
               ; return (Tag key val) }

data PartialOption = OptLang ELanguage
                   | OptBuffer EBuffer
                   | OptTime ETimeRep
                   | OptRuntime ERuntime
                   | OptTags [ETag]
                   deriving (Eq, Show)

optionParser :: Parser PartialOption
optionParser = (do { resvd "time"
                   ; timeOpt <- timeOption
                   ; return (OptTime timeOpt) })
               <|> (do { resvd "runtime"
                       ; run <- runtimeOption
                       ; return (OptRuntime run) })
               <|> (do { tag <- tagOption
                       ; return (OptTags [tag]) })
               <?> "option type: time, runtime, tag"

optionSplitter :: PartialOption -> ([ELanguage], [EBuffer], [ETimeRep], [ERuntime], [[ETag]])
optionSplitter (OptLang l)    = ([l], [], [], [], [])
optionSplitter (OptBuffer b)  = ([], [b], [], [], [])
optionSplitter (OptTime t)    = ([], [], [t], [], [])
optionSplitter (OptRuntime r) = ([], [], [], [r], [])
optionSplitter (OptTags ts)   = ([], [], [], [], [ts])

defaultEmit = EmitOptions { eBuffer = (EBuffer "unlisted" Nothing)
                          , eLanguage = ELangCpp
                          , eTimeRep = (ETimeSpec ETimeClockMonotonic)
                          , eRuntime = (ERuntime False)
                          , eTags = [] }

optionCombine :: [PartialOption] -> Either String EmitOptions
optionCombine opts = let
  addUp (a, b, c, d, e) (f, g, h, i, j) =
    (a ++ f, b ++ g, c ++ h, d ++ i, e ++ j)
  (langs, buffers, timereps, runtimes, rawTags) =
    foldl' addUp ([], [], [], [], []) (
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
  in (EmitOptions
      <$> max1 buffers "buffer line" (EBuffer "unlisted" Nothing)
      <*> exact1 langs "Language"
      <*> max1 timereps "time representation" (
         ETimeSpec ETimeClockMonotonic)
      <*> max1 runtimes "multithread option" (ERuntime False)
      <*> (pure tags))

parseHead :: Parser PartialOption
parseHead = (resvd "option" >> optionParser)
            <|> (do { r <- emitCmd ; return $OptLang r})
            <|> (do { r <- bufferCmd; return$ OptBuffer r})
            <?> "header matter"

headParser :: Parser EmitOptions
headParser = do { options <- many parseHead
                ; case (optionCombine $ options) of
                    Left msg -> fail msg
                    Right opts -> return opts
                }


parseText :: Show a => Parser a -> String -> String -> Either ParseError a
parseText p input fname = parse (do { ws
                                    ; x <- p
                                    ; eof
                                    ; return x
                                    }) fname input

tparse :: Show a => Parser a -> String -> Either ParseError a
tparse p input = parseText p input "input"

fileParser = do { ws
                ; head <- headParser
                ; frames <- many frame
                ; return (Buffer head frames) }

parseFile :: String -> IO (Either String Buffer)
parseFile fname = do  {
    let showLeft :: Show a => Either a b -> Either String b
        showLeft (Left a) = Left $ show a
        showLeft (Right b) = Right b
        showErr :: String -> IOError -> IO (Either String a)
        showErr fname ex = return $ Left $ fname ++ ": " ++ show ex
  ; res <- handle (showErr fname) (do {
    filetext <- readFile fname ;
    return $ showLeft $ parse fileParser fname filetext
    })
  ; return res
  }


-- |Read a buffer from a JSON string
readBuffer :: String -> Maybe Buffer
readBuffer s = decode (pack s)

-- |Show a buffer to a JSON string
saveBuffer :: Buffer -> String
saveBuffer b = unpack $ encode b


