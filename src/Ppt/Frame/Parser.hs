{-# LANGUAGE TemplateHaskell #-}
module Ppt.Frame.Parser where -- (parseFile, compileFrame) where

import Control.Exception                      (handle)
import Control.Lens                           hiding (element, noneOf)
import Control.Monad
import Control.Monad.Trans.Either
import Data.Aeson                             (decode, encode)
import Data.ByteString.Lazy.Char8             (pack, unpack)
import Data.List
import Data.Maybe                             (catMaybes, mapMaybe)
import Ppt.Frame.ParsedRep
import Text.ParserCombinators.Parsec          (GenParser, ParseError, alphaNum, char,
                                               digit, eof, many, many1, noneOf, sepBy1,
                                               string, try, (<?>), (<|>))
import Text.ParserCombinators.Parsec.Language (javaStyle)
import Text.ParserCombinators.Parsec.Prim     (parse)

import qualified Control.Lens.Fold                      as CLF
import qualified Data.HashMap.Strict                    as HM
import qualified Text.ParserCombinators.Parsec.Token    as P

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

primType o = ( resvd "double" >> return PDouble )
             <|> ( resvd "int" >> return PInt )
             <|> ( resvd "float" >> return PFloat )
             <|> ( resvd "time" >> return (PTime (_eTimeRep o)))
             <|> ( resvd "counter" >> return (PCounter Nothing))
             <?> "type name"
-- TODO: replace primType here with a higher-level production that
-- also takes interval and other keywords.  They form a list of
-- qualified-type-specifiers, which get processed by another function
-- into the FMemberElem.
element :: Bool -> EmitOptions -> Parser [FrameElement]
element mul o = do { typ <- primType o
                   ; names <- identifier `sepBy1` comma
                   ; semi
                   ; return (map (\n -> FMemberElem $ FMember typ n mul) names)
                   }

-- |Primary member parser.  This will need some additions over time.
-- The syntax will be:
-- - interval time duration;
-- - int count;
-- - [live] [exported] stat field = gap(duration) / count
frameMember :: EmitOptions -> Parser [FrameElement]
frameMember opts = do { try (resvd "interval" >> (element True opts)) }
                   <|> (element False opts)
                   <?> "member declaration"

frame opts = do { resvd "frame"
                ; name <- identifier
                ; ch '{'
                ; ws
                ; members <- many (frameMember opts)
                ; ws
                ; ch '}'
                ; ws
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
              ; semi
              ; return lang
              }

bufferValue = ( resvd "default" >> return Nothing )
              <|> do { ds <- many1 digit; ws; return (Just (read ds)) }

bufferCmd = do { resvd "buffer"
               ; name <- identifier
               ; size <- bufferValue
               ; ws
               ; semi
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

-- TODO(lally): EmitOptions *should* have each type of EOption as a
-- full-fledged member (unless I leave it as a secondary 'misc'
-- bucket, or a generator config, I donno.).  But right now I'm saving
-- time to get this out.
data PartialOption = OptLang { _lang :: ELanguage }
                   | OptBuffer { _buffer :: EBuffer }
                   | OptTime { _time :: ETimeRep }
                   | OptRuntime { _runtime :: ERuntime }
                   | OptTags { _tags :: [ETag] }
                   | OptCounter { _isNative :: Bool }
                   | OptDebug { _isDebug :: Bool }
                   deriving (Eq, Show)

makeLenses ''PartialOption

optionParser :: Parser PartialOption
optionParser = (do { resvd "time"
                   ; timeOpt <- timeOption
                   ; ws
                   ; semi
                   ; return (OptTime timeOpt) })
               <|> (do { resvd "runtime"
                       ; run <- runtimeOption
                       ; ws
                       ; semi
                       ; return (OptRuntime run) })
               <|> (do { resvd "native_counter"
                       ; ws
                       ; run <- boolOption
                       ; semi
                       ; return (OptCounter run) })
               <|> (do { resvd "debug"
                       ; ws
                       ; run <- boolOption
                       ; semi
                       ; return (OptDebug run) })
               <|> (do { tag <- tagOption
                       ; ws
                       ; semi
                       ; return (OptTags [tag]) })
               <?> "option type: time, runtime, tag"

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


optionCombine :: [PartialOption] -> Either String EmitOptions
optionCombine opts = let
  langs = catMaybes $ map (preview lang) opts
  buffers = catMaybes $ map (preview buffer) opts
  timereps = catMaybes $ map (preview time) opts
  runtimes = catMaybes $ map (preview runtime) opts
  lastBool lens = let list = catMaybes $ map (preview lens) opts
                      len = length list
                  in drop (max 0 (len-1)) list
  -- For other runtime opts, concatenate the lists created in this form
  counters = map ENativeCounter $ lastBool isNative
  debugOpts = map EDebug $ lastBool isDebug
  theTags = concat $  catMaybes $ map (preview tags) opts
  in (EmitOptions
      <$> max1 buffers "buffer line" (EBuffer "unlisted" Nothing)
      <*> exact1 langs "Language"
      <*> max1 timereps "Time representation" (
         ETimeSpec ETimeClockMonotonic)
      <*> max1 runtimes "multithread option" (ERuntime False)
      <*> (pure theTags)
      <*> (pure (counters ++ debugOpts)))

optionUpdate :: EmitOptions -> [PartialOption] -> EmitOptions
optionUpdate base opts =
  let mLast [] = Nothing
      mLast xs = Just (last xs)
      -- Pattern:
      -- 'ins' here will be 'opts' above.
      replace inpl outl ins outs =
        let override = lastOf (traverse . inpl) ins
        in maybe (view outl outs) id override
      concatenate inpl outl ins outs =
        catMaybes [preview outl outs, preview inpl ins]
      updateTags ins outs =
        let decompose (Tag k v) = (k, v)
            compose (k, v) = Tag k v
            origTags :: HM.HashMap String String
            origTags = HM.fromList $ map decompose $ view eTags outs
            newTags :: [(String, String)]
            newTags = map decompose $ concat $ mapMaybe (preview tags) ins
            combined = foldl (\hm (k, v) -> HM.insert k v hm) origTags newTags
        in map compose $ HM.toList combined

      langs = catMaybes $ map (preview lang) opts
      lang_ = replace lang eLanguage opts base
      buffers = catMaybes $ map (preview buffer) opts
      buffer_ = replace buffer eBuffer opts base
      timereps = catMaybes $ map (preview time) opts
      timeRep_ = replace time eTimeRep opts base
      runtimes = catMaybes $ map (preview runtime) opts
      runtime_ = replace runtime eRuntime opts base
      theTags = concat $  catMaybes $ map (preview tags) opts
      tags_ = updateTags opts base
  in EmitOptions buffer_ lang_ timeRep_ runtime_  tags_ (_eOptions base)

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

fileParser :: [PartialOption] -> Parser Buffer
fileParser opts = do { ws
                     ; head <- headParser
                     ; let eopts = optionUpdate head opts
                     ; frames <- many (frame eopts)
                     ; return (Buffer head frames) }

parseFile :: String -> [PartialOption] -> IO (Either String Buffer)
parseFile fname opts = do  {
    let showLeft :: Show a => Either a b -> Either String b
        showLeft (Left a) = Left $ show a
        showLeft (Right b) = Right b
        showErr :: String -> IOError -> IO (Either String a)
        showErr fname ex = return $ Left $ fname ++ ": " ++ show ex
  ; res <- handle (showErr fname) (do {
    filetext <- readFile fname ;
    return $ showLeft $ parse (fileParser opts)  fname filetext
    })
  ; return res
  }


-- |Read a buffer from a JSON string
readBuffer :: String -> Maybe Buffer
readBuffer s = decode (pack s)

-- |Show a buffer to a JSON string
saveBuffer :: Buffer -> String
saveBuffer b = unpack $ encode b

