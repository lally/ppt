import Text.ParserCombinators.Parsec

data FrameType = fDouble | fFloat | fInt;

data FrameElement = FrameElement FrameType String
data FrameSpecification = FrameSpecification String [FrameElement]
data EmissionSpec = LangC | LangCpp
data FullSpecification = Spec EmissionSpec [FrameSpecification]

--emitType :: GenParser Char st EmissionSpec
--
-- Note: I'm not splitting out the whitespace.  Go a little higher level.
-- Then,  finally emit my struct decls & the 
emitC = string "C"
emitCpp = string "Cpp"
emitType = string "emit" >> ( emitC <|> emitCpp )

frameSpec = (string "frame") >> 

frameSpecs = many frameSpec

commandFile :: GenParser Char st FulLSpecification
commandFile =
            do emitType <- emissionSpec
               frames <- frameSpecs
               return (Spec emitType frames)
               