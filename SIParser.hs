module SIParser ( parseText, commandFile, PadBlock, buildPBs, implement, quickParseFile ) where

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
              <|> do { ds <- many1 digit; SIParser.whiteSpace; return (Just (read ds)) }

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

quickParseFile :: String -> IO (Either ParseError FullSpecification)
quickParseFile fname = do text <- readFile fname
                          return (parseText commandFile text fname)

------------------------------------------------------------
-- In-memory layout


-- [elements in right now] (current size)
data PadBlock = PB [ImplMember] Int
                deriving Show


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

--  'buildPBs' works by first sorting the elements by (decreasing)
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
scanWhile predicate [] init (pcount, phead) = (pcount, phead, [])
scanWhile predicate [x] init (pcount, phead) = 
          let (pres, ninit) = predicate x init
           in if pres then
                (pcount, (phead ++ [x]), [])
              else
                (pcount, phead, [x])

scanWhile predicate xm@(x:xs) init (pcount, phead) =
          let (pres, ninit) = predicate x init
           in if pres then
                 scanWhile predicate xs ninit (pcount+1, phead ++ [x])
              else
                 (pcount, phead, xm)


-- Repeatedly consume a few input elements and merge them into a new output
-- element.  Use 'pred' and its argument 'pinit' to determine which elements go
-- into a block: 'pred' returns true for every element that should go into the
-- block.  'pinit' is the first value of the second argument to 'pred'.  Each
-- call's resulting tuple's second argument will be used as that second
-- argument in subsequent calls.
blockifyList :: (a -> b -> (Bool, b)) -> ([a] -> c) -> [a] -> b -> [c]
blockifyList pred merge [] pinit = []
blockifyList pred merge elems pinit =
             let (n, head, tail) = scanWhile pred elems pinit (0, [])
              in (merge head) : (blockifyList pred merge tail pinit)


{-
   We consume ImplMembers and put them into PadBlocks, up to 'align' size each.
   We return both the list and the last element's length.
-}
-- Convert a frame's elements into a list of pad-blocks.  When determining the
-- final layout, the individual blocks will need IMPad elements added to make
-- them sum up to 'maxSz' bytes large.
buildPBs :: RunConfig -> Int -> [ImplMember] -> [FrameElement] -> ([PadBlock], Int)
buildPBs cfg align header mems = 
         let imems = mapElements mems  
             elemsBySize = sortByAscendingSizes cfg imems
             fitsInPB :: ImplMember -> Int -> (Bool, Int)
             fitsInPB elem budget = 
                      let remain = budget - implSize cfg elem
                       in (remain >= 0, remain)
             makePB :: [ImplMember] -> PadBlock
             makePB elems = PB elems (sum $ map (implSize cfg) elems)
             -- build the list of the blocks
             allBlocks = blockifyList fitsInPB makePB (header ++ imems) align
             -- now get the last block and see if we can put the footer on there.
             (PB _ lastblksz) = head $ reverse allBlocks
          in (allBlocks, lastblksz)

data PBImplFrame = PBImpl String [PadBlock]
                   deriving Show

-- Add additional empty PBs to the input PBImplFrame to make it fill 
padAsNeeded :: Int -> PBImplFrame -> PBImplFrame
padAsNeeded maxPBs pif@(PBImpl fnm elems) = 
            PBImpl fnm (elems ++ (take (maxPBs - (length elems)) $ repeat (PB [] 0)))


-- insert padding 
padding 0 = []
padding ln = [ImplMember Nothing (IMPad ln)]

-- Add padding to each element
fillElements :: Int -> Int -> [PadBlock] -> [ImplMember] -> [PadBlock]
fillElements _ _ [] _ = []
fillElements align footerlen [x@(PB pmems psz)] footer = 
             if footerlen < (align - psz) then
                if (align - psz) > footerlen then
                   let listFront = pmems ++ (padding (align - psz))
                    in [PB (listFront ++ footer) align]
                else
                   [PB (pmems ++ footer) align]
             else
                x:[(PB ((padding (align - footerlen)) ++ footer) align)]

fillElements align f (x@(PB pmems psz):xs) footer = 
             (PB (pmems ++ (padding (align - psz))) align) : (fillElements align f xs footer)

{- Given a configuration, header, footer, and input list of frames, put out a
   list of implementation frames. 

   (1) get the alignment
   (2) reorder & sort the packets
   (3) determine final length
   (4) pad & add footer.
   -}

-- Determine the minimum PBs needed to fit the footer in each element.
minFooterOffset :: RunConfig -> Int -> ([PadBlock], Int) -> Int
minFooterOffset cfg footerlen (elems, len) = 
                let elemlen = length elems
                 in if len >= footerlen then elemlen else (elemlen + 1)

bracketPBs :: RunConfig -> ([ImplMember],[ImplMember]) -> [Frame] -> [ImplFrame]
bracketPBs cfg (header,footer) elems =
           let minAlign (Frame _ fe) = maxSize cfg $ mapElements fe
               align = maximum $ map minAlign elems
               footerlen = sum $ map (implSize cfg) footer

               -- Run the conversion and save the free-size for a later unzip &
               -- minimum call.
               pbImpl (Frame fnm fe) = 
                      let (elems, sz) = buildPBs cfg align header fe
                       in (PBImpl fnm elems, sz)
               (pbsOfAll, sizes) = unzip $ map pbImpl elems
               -- given a list of free-sizes elements, look for the longest
               -- element and see if we could fit the footer in there.
               minPBs = minimum sizes
               pbPaddedFrames :: [PBImplFrame]
               pbPaddedFrames = map (padAsNeeded minPBs) pbsOfAll
               paddedPBs (PBImpl fnm pbs) = 
                         let filledPbs = fillElements align footerlen pbs footer
                             pullImpls (PB ms _) = ms
                          in ImplFrame fnm (concatMap pullImpls filledPbs)
            in map paddedPBs pbPaddedFrames

-- Add IMPad elements to each padblock to make them whole, then
-- concatenate their elements together into the full
-- (internally-padded) list.  Note that the last element of the
-- original PB list shouldn't go here, as it needs special processing
-- to have the footer attached.
padAndConcat :: RunConfig -> Int -> [ImplMember] -> [PadBlock] -> [ImplMember]
padAndConcat cfg sz footer elems = 
             let  -- Pad the block if necessary, but definitely break it out.
                 padABlock (PB mems memsz) = 
                           if memsz < sz then
                              let padding = ImplMember Nothing (IMPad (sz - memsz))
                               in mems ++ [padding]
                           else
                              mems
              in concatMap padABlock elems



implement :: RunConfig -> FullSpecification -> FullImplementation
implement cfg spec@(Spec emit (Buffer nm _ _) frames) = 
          let header | length frames > 1 = 
                     [ImplMember Nothing (IMSeqno SFront), ImplMember Nothing IMDescriminator]
                     | otherwise = [ImplMember Nothing (IMSeqno SFront)]
              allElems = concatMap (\(Frame _ es) -> es) frames
              footer = [ImplMember Nothing (IMSeqno SBack)]
           in Impl emit nm (bracketPBs cfg (header, footer) frames)
