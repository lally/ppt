module SIParser ( parseText, commandFile, PadBlock(PB), buildPBs, implement, quickParseFile, 
       sortByAscendingSizes, mapElements, blockifyList, makePadBlock, bracketPBs, maxSize,
       tailPadAndAppend, PBImplFrame(PBImpl), padABlock, padding, aligningPadding, scanWhile ) where

import Text.ParserCombinators.Parsec (sepBy, try, char, eof, many1,
                                      many, ParseError, digit,
                                      string, (<|>), (<?>), GenParser)
import Text.ParserCombinators.Parsec.Language (javaStyle)
import Text.ParserCombinators.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec.Token as P
import List (sortBy)
import Configuration (RunConfig, implSize)
import StaticInstrumentation
--import Debug.Trace(trace, traceShow)
    
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

makePadBlock mems n = PB mems n

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
            | (implSize cfg a) <= (implSize cfg b) = LT
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
blockifyList :: Show a => (a -> b -> (Bool, b)) -> ([a] -> c) -> [a] -> b -> [c]
blockifyList pred merge [] pinit = []
blockifyList pred merge elems pinit =
             let (n, head, tail) = scanWhile pred elems pinit (0, [])
              in (merge head) : (blockifyList pred merge tail pinit)

-- insert padding 
padding 0 = []
padding ln | ln > 0 = [ImplMember Nothing (IMPad ln)]
           | otherwise = fail "Negative padding"

-- Determine how much up-front padding is needed for a member at a
-- given offset to align it properly
aligningPadding :: RunConfig -> ImplMember -> Int -> Int
aligningPadding cfg mem@(ImplMember fe ty) offset 
                | offset `mod` (implSize cfg mem) == 0 = 0
                | otherwise = let sz = implSize cfg mem in sz - (offset `mod` sz)

{- 
   We consume ImplMembers and put them into PadBlocks, up to 'align' size each.
   We return both the list and the last element's length.
-}
-- Convert a frame's elements into a list of pad-blocks.  When determining the
-- final layout, the individual blocks will need IMPad elements added to make
-- them sum up to 'maxSz' bytes large.
buildPBs :: RunConfig -> Int -> [ImplMember] -> [FrameElement] -> [PadBlock]
buildPBs cfg align header mems = 
         let elemsBySize = sortByAscendingSizes cfg $ mapElements mems
            
             -- The budget is also an offset counter.
             fitsInPB :: Int -> ImplMember -> Int -> (Bool, Int)
             fitsInPB align elem used = 
                 -- Our budget has to be hit for any padding bytes necessary.
                 let elemSz = (aligningPadding cfg elem used) + implSize cfg elem
                     usedAfter = used + elemSz
                     remain = align - usedAfter
                 in (remain >= 0, usedAfter)

             -- simple c'tor for PadBlocks.
             makePB :: [ImplMember] -> PadBlock
             makePB elems = PB elems (sum $ map (implSize cfg) elems)

             -- build the list of the blocks
          in blockifyList (fitsInPB align) makePB (header ++ elemsBySize) 0



-- Apply inline padding to a block.  Return the padded block, and the
-- amount of free space at the end.
padABlock :: RunConfig -> Int -> PadBlock -> (PadBlock, Int)
padABlock cfg align block@(PB members size) = 
          let insertPadding offset [] = ([], align - offset)
              insertPadding offset ((mem@(ImplMember fe ty)):mems) =
                            let padAmount = aligningPadding cfg mem offset
                                sz = implSize cfg mem
                                (tail, remain) = insertPadding (offset + (padAmount + sz)) mems
                             in ((padding padAmount) ++ [mem] ++ tail, remain)
              (paddedMems, remainder) = insertPadding 0 members
           in (PB (paddedMems) (align - remainder), remainder)

-- Given a paremeter k, tail-pad the first k elements of an input list
-- of pad-blocks.  If k > length(input_list), add all-padding blocks
-- to make it of proper length, putting a pad-and-footer block at the end.
-- If k < length(input_list), put the footer at the end of the last block.
-- k is the number of padblocks that don't contain the footer.
tailPadAndAppend :: RunConfig -> Int -> Int -> [ImplMember] -> [PadBlock] -> [PadBlock]
tailPadAndAppend cfg align k footer [] 
                 | k == 0 = 
                   let footerlen = sum $ map (implSize cfg) footer
                   in [PB ((padding (align - footerlen)) ++ footer) align]
                 | k > 0 =
                   (PB (padding align) align):(tailPadAndAppend cfg align (k-1) footer [])
                 | otherwise =
                   fail ("tailPadAndAppend (no mems): Invalid value for k: " ++ show k)

tailPadAndAppend cfg align k footer pbs@((x@(PB mems sz)):remain) 
                 | k == 0 =
                   let footerlen = sum $ map (implSize cfg) footer
                    in [PB (mems ++ (padding (sz - footerlen)) ++ footer) align]
                 | k > 0 = 
                   (PB (mems ++ (padding (align - sz))) align):(
                             tailPadAndAppend cfg align (k-1) footer remain)
                 | otherwise =
                   fail ("tailPadAndAppend (" ++ 
                          (show $ length pbs) ++ " members) Invalid value for k: " ++ show k)

-- test situations for tailPadAndAppend:
                 
-- The stuff above is the new, clear idea of a two-pass system:
-- Pass 1: Internal padding and tail-freespace determination
-- (here, determine the maximum offset of the footer)
-- Pass 2: tail-padding, list padding (add full-padding blocks), footer append
-- Profit!

-- stuff below should be rewritten to work with this stuff above, and
-- will need re-jiggering on the way to make it work.

{- Given a configuration, header, footer, and input list of frames, put out a
   list of implementation frames. 
   -}


-- A temporary record type, used for bookkeeping the pad-blocks that
-- contain the members: PBImpl (name of frame) (pad blocks) (length of
-- pad blocks) (space at end of last pad block)
data PBImplFrame = PBImpl String [PadBlock] Int Int
                   deriving Show

-- Determine the minimum PBs needed to fit the footer in each element.

bracketPBs :: RunConfig -> ([ImplMember],[ImplMember]) -> [Frame] -> [ImplFrame]
bracketPBs cfg (header,footer) elems =
           let minAlign (Frame _ fe) = maxSize cfg $ mapElements fe
               align = maximum $ map minAlign elems
               footerlen = sum $ map (implSize cfg) footer

               pbImpl (Frame fnm fe) = 
                      let elems = buildPBs cfg align header fe
                       in (fnm, elems)

               padFrame :: (String, [PadBlock]) -> PBImplFrame
               padFrame (fname, felems) =
                        let (padded, frees) = unzip $ map (padABlock cfg align) felems
                            tailFree = last frees
                         in (PBImpl fname padded (length padded) tailFree)

               pbPaddedFrames = map (padFrame . pbImpl) elems

               kValue (PBImpl _ _ blocks freebytes) 
                   | freebytes >= footerlen = blocks-1
                   | otherwise = blocks
               
               k = maximum $ map kValue pbPaddedFrames

               finishPB (PBImpl fname felems sz free) =
                   let blocks = tailPadAndAppend cfg align k footer felems
                       pullImpls (PB ms _) = ms
                   in ImplFrame fname (concatMap pullImpls blocks)
            in map finishPB pbPaddedFrames

implement :: RunConfig -> FullSpecification -> FullImplementation
implement cfg spec@(Spec emit (Buffer nm _ _) frames) = 
          let header | length frames > 1 = 
                     [ImplMember Nothing (IMSeqno SFront), ImplMember Nothing IMDescriminator]
                     | otherwise = [ImplMember Nothing (IMSeqno SFront)]
              footer = [ImplMember Nothing (IMSeqno SBack)]
           in Impl emit nm (bracketPBs cfg (header, footer) frames)
