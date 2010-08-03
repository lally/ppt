import Text.ParserCombinators.Parsec (sepBy, try, char, eof, many, ParseError, string, (<|>), (<?>), GenParser)
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec.Token as P
import List (sortBy)

--
-- Parser types
-- 

--- Note: these are *really* simplistic types.  I'd like to pop in an
--- 'average' field here.  But that's less-frequent data (and a little
--- cooked), so let's save that for later.

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

element = do { typ <- elementType
             ; name <- Main.identifier
             ; Main.semi
             ; return (FrameElement typ name)
             }

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

------------------------------------------------------------
-- In-memory layout



-- The cache line size we're optimizing for.
cacheLineSize = 64 

-- The minimal alignment of this architecture
alignment :: Int
alignment = 4

-- sizes of types
sizeof :: FrameType -> Int
sizeof FDouble = 8
sizeof FFloat = 4
sizeof FInt = 4

{- ALGORITHM

Requirements:
 - Some elements are *paired*, requiring that they all fit in the same cache-
   line
 - Minimize the number of cache lines required.
 - We can reorder elements at will: only the final ordering matters.

Algorithm:
 - Start off with the optimal situation for unpaired types: sort by size,
   then you minimize padding. Call this the primary set
 - Then, as long as there are pairs:
   - Pull a pair out, place it into its own cache line at the front.  Make it
     part of the 'candidate set'
   - Merge any candidates together that'll fit together in a cache line.
 - Define the final structure as a set of inner structures, padded to 64-byte
   boundaries. 

FOR NOW
 - no pairing.  Just sort by size.
-}

elemSize :: FrameElement -> Int
elemSize (FrameElement a _) = sizeof a

allocateSizes :: FrameSpecification -> [FrameElement]
allocateSizes (FrameSpecification _ elems) =
    let ordering a b
            | (elemSize a) < (elemSize b) = LT
            | otherwise = GT
     in sortBy ordering elems

data Allocation = Allocation {
      offset :: Int -- relative to the containing allcoator.
    , frameElem :: FrameElement 
    } deriving Show 

mapSizes :: [FrameElement] -> [Allocation]
mapSizes [] = []
mapSizes frames =
    let modUp off amt = 
            (1+ (off `div` amt)) * amt
        changeType off priorSize [] = []
        changeType off priorSize (x:xs) 
                   | priorSize < (elemSize x) = 
                       let sz = elemSize x
                           nextoff = modUp off (max alignment sz)
                           xalloc = Allocation { offset = nextoff, frameElem = x }
                        in xalloc:(changeType (nextoff + sz) sz xs)
                   | otherwise = 
                       let xalloc = Allocation { offset = off, frameElem = x }
                           sz = elemSize x
                        in xalloc:(changeType (off + sz) sz xs)
        firstSize = elemSize (head frames)
     in changeType 0 firstSize frames



showAllocations :: String -> [Allocation]
showAllocations text = 
    case testParse frameSpec text of
      Right (FrameSpecification _ xs) ->
          mapSizes xs
      Left _ -> []

