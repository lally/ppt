module SIParser ( parseText, commandFile, PadBlock, buildPBs, implement ) where

import Text.ParserCombinators.Parsec (sepBy, try, char, eof,
                                      many, ParseError,
                                      string, (<|>), (<?>), GenParser)
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec.Token as P
import List (sortBy)
import Configuration
import StaticInstrumentation
    
--
-- Parser types
-- 


lexer :: TokenParser ()
lexer = makeTokenParser 
         (javaStyle {
            reservedNames = [ "emit", "frame", "double", "int", "float", "time" ]
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

ch c = do { SIParser.whiteSpace; x <- char c; SIParser.whiteSpace; return x }

emitType = do { SIParser.reserved "emit" 
              ; SIParser.whiteSpace
              ; lang <- emitCmd
              ; SIParser.whiteSpace
              ; return lang
              }

elementType = ( SIParser.reserved "double" >> return FDouble )
              <|> ( SIParser.reserved "int" >> return FInt )
              <|> ( SIParser.reserved "float" >> return FFloat )
              <|> ( SIParser.reserved "time" >> return FTime )
              <?> "type name"

element = do { typ <- elementType
             ; name <- SIParser.identifier
             ; SIParser.semi
             ; return (FrameElement typ name)
             }

frameSpec = do { SIParser.reserved "frame"
               ; name <- SIParser.identifier
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
               

parseText :: Show a => Parser a -> String -> String -> Either ParseError a
parseText p input fname = parse (do { SIParser.whiteSpace
                            ; x <- p
                            ; eof
                            ; return x
                            }) fname input

------------------------------------------------------------
-- In-memory layout


-- [elements in right now] (current size)
data PadBlock = PB [ImplMember] Int


maxSize :: RunConfig -> [ImplMember] -> Int
maxSize _ [] = 0
maxSize c xs = maximum $ map (implSize c) xs

memPrefix :: RunConfig -> [ImplMember] -> Int -> ([ImplMember], [ImplMember])
memPrefix cfg ms n = memPrefix' ms n []
          where 
          -- memPrefix :: remainder -> max sz -> current list -> (new current list, new remainder)
          memPrefix' :: [ImplMember] -> Int -> [ImplMember] -> ([ImplMember], [ImplMember])
          memPrefix' [] n ts | n > 0 = (ts ++ [(ImplMember Nothing (IMPad n))], [])
                             | otherwise = (ts, [])
          memPrefix' ms 0 ts = (ts, ms)
          memPrefix' r@(m:ms) n ts | sz <= n = memPrefix' ms (n - sz) (m:ts)
                                   | otherwise = (ts ++ [(ImplMember Nothing (IMPad n))], r)
                                   where sz = implSize cfg m


mapElements :: [FrameElement] -> [ImplMember]
mapElements xs = map mapElement xs
            where mapElement e@(FrameElement t n) = ImplMember (Just e) (mapType t)
                  mapType FDouble = IMDouble
                  mapType FFloat = IMFloat
                  mapType FInt = IMInt
                  mapType FTime = IMTime

sortByAscendingSizes :: RunConfig -> [ImplMember] -> [ImplMember]
sortByAscendingSizes cfg elems = 
    let ordering a b
            | (implSize cfg a) > (implSize cfg b) = LT
            | otherwise = GT
     in sortBy ordering elems

--  buildPBs
--  ========

--  'buildPBs' works by first sorting the elements by (increasing)
-- size, determine the largest item, then tightly-pack (and align)
-- these objects.  The procedure is not terribly different from what a
-- compiler normally does, but this way we get exact alignments for
-- the sake of the LLVM code.

--  After determining the largest element, we build a sequence of
-- pad-blocks.  Every element will be allocated to a pad-block.
-- Pad-blocks will have as many elements as we can fit, with
-- additional padding bytes added as needed.  Each pad-block is the
-- size of the largest single type, so that a value of the largest
-- type is both alone in its block and aligned to its natural size (by
-- virtue of the pad-blocks being aligned to the same modulus).

-- We're also taking advantage of the fact that all the sizes are
-- powers of two.  So, we'll just need two of a single type to fill in
-- the next-largest pad-block size.  We may have to repeat.  If we run
-- out, we'll just add in pad blocks.

buildPBs :: RunConfig -> [FrameElement] -> [PadBlock]
buildPBs cfg elems@(e:es) =  
         let max a b = if a < b then b else a
             header = ImplMember Nothing IMSeqno
             implmems = header : (sortByAscendingSizes cfg (mapElements elems))
             biggest_elem_sz = max (implSize cfg header) (maxSize cfg implmems)
             blockify (l, []) = [PB l biggest_elem_sz]
             blockify (l,r) = (PB l biggest_elem_sz) : blockify (memPrefix cfg r biggest_elem_sz)
         in
             blockify (memPrefix cfg implmems biggest_elem_sz)

implement :: RunConfig -> FullSpecification -> FullImplementation
implement cfg spec@(Spec emit fs) = Impl emit $ map implFrame fs
          where implFrame f@(FrameSpecification nm elems) = FrameImpl nm $ concatMap breakPBs (buildPBs cfg elems)
                breakPBs pb@(PB ms _) = ms
                