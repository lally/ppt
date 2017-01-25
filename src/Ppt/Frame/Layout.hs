module Ppt.Layout where
import Ppt.BufferRep

sizeOf :: TargetInfo -> Primitive -> Int
sizeOf info PDouble = tDouble info
sizeOf info PFloat = tFloat info
sizeOf info PInt = tInt info
sizeOf info PTime = tTime info
sizeOf info PCounter = tTime info

serializeOffsets :: TargetInfo -> [LayoutMember] -> [LayoutMember]
serializeOffsets target inmems =
  let serialize :: TargetInfo -> Int -> [LayoutMember] -> [LayoutMember]
      serialize tinfo pfx (m:mems) =
        let nextElemSize = pfx + (sizeOf tinfo (lType m))
        in (m {lOffset = pfx }):(serialize tinfo nextElemSize mems)
  in serialize target 0 inmems

{-
  Things to consider:
   - The layout process has to insert:
     - The front/back sequence numbers
     - The type descriminator
   - The layout representation has to wrap up the type descriminator too.
-}

diffName nm 0 = nm ++ "__start"
diffName nm 1 = nm ++ "__end"

-- Lay out individual frames
compileFrame :: TargetInfo -> EmitOptions -> Frame -> LayoutMember
compileFrame target (Emit _ _  timerep runtime _) (Frame _ elems) =
  let baseLayout elem@(FMemberElem (FMember ty nm True)) =
        let sz = sizeOf target ty
            elm = Just elem
        in [
          LMember 0 ty True sz sz elm (Just 0) (diffName nm 0),
          LMember 0 ty True sz sz elm (Just 1) (diffName nm 1)
         ]
      baseLayout elem@(FMemberElem (FMember ty nm False)) =
        let sz = sizeOf target ty
            elm = Just elem
        in [LMember 0 ty True sz sz elm Nothing nm]
      baseLayout elem@(FCalculatedElem _ _ _ _ _) = []
  in undefined

-- Then get theem padded to the same size.
