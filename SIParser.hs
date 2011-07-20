module SIParser ( parseText, commandFile, PadBlock, buildPBs, implement ) where

import Text.ParserCombinators.Parsec (sepBy, try, char, eof, many1,
                                      many, ParseError, digit,
                                      string, (<|>), (<?>), GenParser)
import Text.ParserCombinators.Parsec.Language (javaStyle)
import Text.ParserCombinators.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec.Token as P
import List (sortBy)
import Configuration (RunConfig, implSize)
import StaticInstrumentation
    
--
-- Parser types
-- 


lexer :: TokenParser ()
lexer = makeTokenParser 
         (javaStyle {
            reservedNames = [ "emit", "frame", "double", "int", "float", "time", "default", "buffer" ]
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

bufferValue = ( SIParser.reserved "default" >> return Nothing ) 
              <|> do { ds <- many1 digit; return (Just (read ds)) }

bufferCmd = do { SIParser.reserved "buffer"
               ; name <- SIParser.identifier
               ; size <- bufferValue
               ; rate <- bufferValue
               ; return (Buffer name size rate)
               }

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
               ; return (Frame name elem)
               }

type Parser t = GenParser Char () t


commandFile :: GenParser Char () FullSpecification
commandFile =
            do { emitType <- emitType
               ; buffer <- bufferCmd
               ; frames <- many frameSpec
               ; return (Spec emitType buffer frames)
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

-- ::  remainder -> max sz -> current list -> (new current list, new remainder)
memPrefix :: RunConfig -> [ImplMember] -> Int -> ([ImplMember], [ImplMember])
memPrefix cfg ms n = memPrefix' ms n []
          where memPrefix' :: [ImplMember] -> Int -> [ImplMember] -> ([ImplMember], [ImplMember])
                memPrefix' [] n ts | n > 0 = (ts ++ [(ImplMember Nothing (IMPad n))], [])
                                           | otherwise = (ts, [])
                memPrefix' ms 0 ts = (ts, ms)
                memPrefix' r@(m:ms) n ts | sz <= n = memPrefix' ms (n - sz) (m:ts)
                                         | otherwise = 
                                             (ts ++ [(ImplMember Nothing (IMPad n))], r)
                                           where sz = implSize cfg m


mapElements :: [FrameElement] -> [ImplMember]
mapElements xs = map mapElement xs
            where mapElement e@(FrameElement t n) = 
                             ImplMember (Just e) (mapType t)
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
-- size of the largest single type (in the entire Frame), so that a
-- value of the largest type is both alone in its block and aligned to
-- its natural size (by virtue of the pad-blocks being aligned to the
-- same modulus).

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
             blockify (l,r) = ((PB l biggest_elem_sz) : 
                      blockify (memPrefix cfg r biggest_elem_sz))
         in
             blockify (memPrefix cfg implmems biggest_elem_sz)


implement :: RunConfig -> FullSpecification -> FullImplementation
implement cfg spec@(Spec emit (Buffer nm _ _) frames) = 
          Impl emit nm $ map implFrame frames
          where implFrame (Frame fname elems) = 
                    ImplFrame fname $ concatMap breakPBs (buildPBs cfg elems)
                breakPBs pb@(PB ms _) = ms
                

-- implement
-- =========

--  'implement' works by constructing layouts for each frame type
--  (with a front sequence number and (if there's more than 1 frame
--  type) discriminator).  It then pads them out to the same length
--  and adds another sequence number at the end.

--  Each frame in the buffer has to be the same size.  They have a
--  sequence number at the front and the back.  If the two sequence
--  numbers do not match, we consider it invalid and ignore it.

--  First, we determine the header type, which is a sequence number
--  possibly followed by a discriminator.  Next, we determine the
--  largest member of any frame, determining our alignment modulus.
--  Then, we build each frame with that header and modulus, (see
--  buldPBs comment above).  Finally, we pad them all out to the same
--  size, and apend the trailing sequence number.

-- Aggregate elements of the first list into elements of the second,
-- using a predicate to choose spans of elements to be fed into a
-- constructor.  The predicate takes a second value, and it returns a
-- new value for that parameter for the next call.
scanWhile :: (a -> b -> (Bool,b)) -> [a] -> b -> (Int, [a]) -> (Int, [a], [a])
scanWhile predicate xm@(x:xs) init (pcount, phead) =
          let (pres, ninit) = predicate x init
           in if pres then
                scanWhile predicate xs ninit (pcount+1, phead ++ [x])
              else
                 (pcount, phead, xm)


-- Repeatedly consume a few input elements and merge them into a new
-- output element.  Use 'pred' and its argument 'pinit' to determine
-- which elements go into a block: 'pred' returns true for every
-- element that should go into the block.  'pinit' is the first value
-- of the second argument to 'pred'.  Each call's resulting tuple's
-- second argument will be used as that second argument in subsequent
-- calls.
blockifyList :: (a -> b -> (Bool, b)) -> ([a] -> c) -> [a] -> b -> [c]
blockifyList pred merge elems pinit =
             let (n, head, tail) = blockifyList pred elems pinit
              in (merge head) ++ (blockifyList pred merge tail)

-- Convert a frame's elements into a list of pad-blocks.  When
-- determining the final layout, the individual blocks will need IMPad
-- elements added to make them sum up to 'maxSz' bytes large.
buildPBs :: RunConfig -> Int -> [ImplMember] -> [FrameElement] -> [PadBlock]
buildPBs cfg maxSz header mems = 
         let imems = mapElements mems  
             elemsBySize = sortByAscendingSizes imems
             fitsInPB elem budget = 
                      let remain = budget - implSize elem
                       in (remain >= 0, remain)
             makePB elems = let szof (ImplMember _ t) = implSize cfg t
                                ttlSize = sum $ map szof elems
                             in PB elems ttlSize
          in blockifyList fitsInPB makePB (header ++ imems) maxSz
             
data PBImplFrame = PBImpl String [PadBlock]
                   deriving Show

-- Given a list of frames, build padblocks for each, and make them all
-- the same size.  Then see if we can put the footer on the last
-- padblock of all of them.  If not, put them in a new one at the end
-- of all of them.  Finally, insert the padding and put out the final
-- ImplFrames.
bracketPBs :: RunConfig -> ([ImplMember],[ImplMember]) -> [Frame] -> [ImplFrame]
bracketPBs cfg (header,footer) elems =
           let minAlign (Frame _ fe) = maxSize cfg fe
               align = maximum $ map minAlign elems
               pbImpl (Frame fnm fe) = 
                       PBImpl fnm (buildPBs cfg align header fe)
               pbsOfAll :: [PBImplFrame]
               pbsOfAll = map pbImpl elems
               maxNrPBs = maximum $ map (\(PBImpl _ e) -> length e) pbsOfAll
               padAsNeeded pif@(PBImpl fnm elems) =
                           let nelems = length elems
                               empty = PB [] align
                               blankPBs = repeat empty (maxNrPBs - nelems)
                            in if nelems < maxNrPBs then
                                 PBImpl fnm (elems ++ blankPBs)
                               else
                                 pif
               pbPaddedFrames :: [PBImplFrame]
               pbPaddedFrames = map padAsNeeded pbsOfAll
               freeSpace (PBImpl _ es) =
                         let (PB _ free) = tail $ reverse es
                          in free
               minFreeSpace = minumum $ map freeSpace pbPaddedFrames
               padding ln = ImplMember Nothing (IMPad ln)
               footerlen = sum $ map implSize footer
               fillElements :: [PadBlock] -> [PadBlock]
               fillElements (x@(PB pmems psz):xs) = (PB (pmems ++ [(padding (align - psz))]) align) : (fillElements xs)
               fillElements [x@(PB pmems psz)] = if maxElemSz < footerlen then
                                                    if align - footerlen > 0 then
                                                      (PB (pmems ++ ((padding (align - psz)):footer)) align) 
                                                    else
                                                      (PB (pmems ++ footer) align) 
                                                 else
                                                    [x:(PB ([padding align - footerlen, footer]) align)]
               paddedPBs (PBImpl fnm pbs) = 
                         let filledPbs = fillElements pbs
                             pullImpls (PadBlock ms _) = ms
                          in ImplFrame fnm (map pullImpls filledPbs)
            in map paddedPBs pbPaddedFrames

-- Add IMPad elements to each padblock to make them whole, then
-- concatenate their elements together into the full
-- (internally-padded) list.  Note that the last element of the
-- original PB list shouldn't go here, as it needs special processing
-- to have the footer attached.
padAndConcat :: RunConfig -> Int -> [PadBlock] -> [ImplMember]
padAndConcat cfg maxSz footer elems = 
             let  -- Pad the block if necessary, but definitely break it out.
                 padABlock (PadBlock mems memsz) sz = 
                           if memsz < sz then
                              let padding = ImplMember Nothing IMPad (sz - memsz)
                               in mems ++ padding
                           else
                              mems
              in concatMap padABlock elems



implement :: RunConfig -> FullSpecification -> FullImplementation
implement cfg spec@(Spec emit (Buffer nm _ _) frames) = 
          let header | length frames > 1 = 
                     [ImplMember Nothing IMSeqno, ImplMember Nothing IMDescriminator]
              header | otherwise = [ImplMember Nothing IMSeqno]
              allElems = concatMap (\(Frame _ es) -> es) frames
              maxElemSz = max $ map implSize allElems
              footer = ImplMember Nothing IMSeqno
           in Impl emit nm (bracetPBs cfg (header, footer) frames)
