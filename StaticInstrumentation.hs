module StaticInstrumentation where
-- Datatype declarations used by a few other modules

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
