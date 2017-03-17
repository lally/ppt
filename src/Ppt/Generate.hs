{-# LANGUAGE OverloadedStrings #-}
module Ppt.Generate where
import Ppt.Configuration
import Ppt.Storage as S

import qualified Ppt.Frame.Parser as P
import qualified Ppt.Frame.ParsedRep as PR
import qualified Ppt.Frame.Layout as L

import System.IO
import Ppt.Generate.C (emitC)
import Ppt.StaticInstrumentation as Inst
import Ppt.SIParser as SIP
import System.Console.GetOpt as GO
import Data.Either
import Data.Char (toUpper)
import Data.Foldable (forM_)
import Data.List.Utils (replace)
import Data.Word (Word8)
import Data.String.Utils (join)
import System.Directory (copyFile)
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import qualified Text.PrettyPrint as PP
import qualified Ppt.Generate.Cp as CP
--import Scratch

import System.Console.GetOpt


generateC :: Inst.FullSpecification -> String
generateC inst = show inst

{-
   Command line processing support
-}
data Flag = OutputFile String
          deriving (Eq, Show)

data GeneratedFile = GF String PP.Doc deriving (Eq, Show)

-- The argument list accepted by the 'generate' command
arglist :: [GO.OptDescr Flag]
arglist = [GO.Option ['o'] ["output"] (GO.ReqArg OutputFile "output") 
           "The output file(s) base name"]


doGenerate :: FullSpecification -> String -> RunConfig -> IO ()
doGenerate spec@(Spec emit _ frames) basefname cfg = do
      let impl = implement cfg spec
--      putStrLn (show impl)
      case emit of
        LangC -> do
          let dstpath = specPath cfg spec
              header_name = (basefname ++ ".h")
              source_name = (basefname ++ ".c")
              converter_name = (basefname ++ "_convert.c")
              creader_name = (basefname ++ "_listen.c")
              (header, source, converter, creader) = 
                emitC cfg spec impl basefname
          makeSpecPath cfg spec
          writeFile (dstpath ++ header_name) header
          writeFile (dstpath ++ source_name) source
          writeFile (dstpath ++ converter_name) converter
          writeFile (dstpath ++ creader_name) creader
          -- L.generateReader cfg spec (dstpath ++ basefname ++ ".ll")
    
        {- If it isn't obvious, this is just a stub -}
        LangCpp -> do
          let dstpath = specPath cfg spec
              header_name = (basefname ++ ".h")
              source_name = (basefname ++ ".cpp")
              converter_name = (basefname ++ "_convert.cpp")
              creader_name = (basefname ++ "_listen.cpp")
              (header, source, converter, creader) = 
                emitC cfg spec impl basefname {- <--- Obvious? -}
          putStrLn ("Note, this is unsupported, and 'checkout' will not " ++ 
                   "work correctly.")
          makeSpecPath cfg spec
          writeFile (dstpath ++ header_name) header
          writeFile (dstpath ++ source_name) source
          writeFile (dstpath ++ converter_name) converter
          writeFile (dstpath ++ creader_name) creader
          -- L.generateReader cfg spec (dstpath ++ basefname ++ ".ll")

--  putStrLn (show result) 
  

generate :: [String] -> RunConfig -> IO ()
generate args cfg =  
  let res = GO.getOpt GO.Permute arglist args in
  case res of
    (opts, files, []) -> do
      let file = head files
      text <- readFile file 
      let result = SIP.parseText SIP.commandFile text file
      case result of
           Left err -> putStrLn ("ERROR: " ++ (show err))
           Right spec@(Spec emit _ frames) ->
                 doGenerate spec basename cfg
                 where
                    isfname (OutputFile _) = True
                    basename = case L.find isfname opts of
                                    Just (OutputFile n) -> n
                                    Nothing -> takeWhile (/= '.') file
    (_, _, _) ->
      putStrLn (show res)
                   

checkout :: [String] -> RunConfig -> IO ()
checkout files cfg = do
      mapM_ (\file -> do
            text <- readFile file
            let result = SIP.parseText SIP.commandFile text file
            case result of
                 Left err -> putStrLn ("ERROR: " ++ (show err))
                 Right spec -> do
                 {- Note that we're assuming C here. -}
                  let dstpath = specPath cfg spec
                      basefilename = takeWhile (/= '.') file
                  mapM_ (\suff -> do
                               let n = (basefilename ++ suff)
                               putStrLn ("Copied " ++ n)
                               copyFile (dstpath ++ n) n) [".h", ".c", ".ll",
                                                           "_convert.c", "_listen.c"]) files

addList :: [String] -> [[String]] -> [[String]]
addList [] prior = prior
addList cur prior = prior ++ [cur]

byOption:: [String]-> [String] -> [[String]] -> [[String]]
byOption (n:ns) cur prior =
  if (length n > 0 &&  n!!0 == '-')
  then byOption ns [n] (addList cur prior)
  else byOption ns (cur ++ [n]) prior

byOption [] cur prior =
  addList cur prior

splitOptionGroups :: [String] -> Maybe ([[String]], String)
splitOptionGroups args =
  if length args > 0
  then
    let (fname:rest_r) = reverse args
        rest = reverse rest_r
        optGroups = byOption rest [] []
    in Just (optGroups, fname)
  else
    Nothing

data GenerateOption = GOFilePrefix String
                    | GOTag String String
                    | GOOption P.PartialOption
                    deriving (Eq, Show)

filePfx (_:arg:[]) = Right $ GOFilePrefix arg
filePfx _ = Left "One argument for --prefix: prefix"

tagPfx (_:k:v:[]) = Right $ GOTag k v
tagPfx _ = Left "Only two arguments for --tag: key value"

mapEither lfn _ (Left l) = Left (lfn l)
mapEither _ rfn (Right r) = Right (rfn r)

optionPfx (o:os) =
  let input = L.intercalate " " (os ++ [";"])
      parseResult = P.parseText P.optionParser input "command line"
  in case parseResult of
    (Left err) -> Left $ show err
    (Right opt) -> Right $ GOOption opt

econcat :: [Either a b] -> Either a [b]
econcat vals =
  let eappend' :: [b] -> [Either a b] -> Either a [b]
      eappend' prior ((Left s):ss) = Left s
      eappend' prior ((Right s):ss) = eappend' (prior ++ [s]) ss
      eappend' prior [] = Right prior
  in eappend' [] vals

pullGroup :: String ->
             ([String] -> Either String a) ->
             [[String]] ->
             (Either String [a], [[String]])
pullGroup tagName fn args =
  let
    (matches, rest) = L.partition (\lst -> length lst >0 && (lst !! 0 == tagName)) args
  in (econcat $ map fn matches, rest)

normalize :: [GenerateOption] -> [P.PartialOption]
normalize ((GOFilePrefix _):ss) = normalize ss
normalize ((GOTag k v):ss) = (P.OptTags [PR.Tag k v]):normalize ss
normalize ((GOOption p):ss) = p:normalize ss
normalize [] = []

{-
mergeOptions :: PR.EmitOptions -> [GenerateOption] -> PR.EmitOptions
mergeOptions base [] = base
mergeOptions base (GOFilePrefix s:args) =
  let pred (PR.EFilePrefix _) = True
      pred _ = False
      filtered = filter (not . pred) $ PR.eOptions base
  in mergeOptions (base { PR.eOptions = (PR.EFilePrefix s):filtered }) args
  
mergeOptions base ((GOTag k v):args) =
  let tagset = HM.insert k v $ HM.fromList $ map (\(PR.Tag k v) -> (k,v)) $ PR.eTags base
      tags = map (\(k, v) -> PR.Tag k v) $ HM.toList tagset
  in mergeOptions (base { PR.eTags = tags }) args
mergeOptions base ((GOOption popt):args) = base
-}

x64Layout = L.TargetInfo 8 4 4 16 8 4


-- Yup, hideous, needs help.
generateCommand :: [String] -> IO ()
generateCommand args =
  let opts = splitOptionGroups args
  -- The following should all be in a runEitherT $ do...
  in case opts of
    Nothing -> putStrLn "ppt generate <usage>"
    Just (groups, fname) -> do
      let (pfx, r1) = pullGroup "--prefix" filePfx groups
          (tags, r2) = pullGroup "--tag" tagPfx r1
          (opts, r3) = pullGroup "--option" optionPfx r2
          result = econcat [pfx, tags, opts]
      case result of
        Left err -> do
          putStrLn (L.concatMap L.concat r1)
          putStrLn (L.concatMap L.concat r2)
          putStrLn (L.concatMap L.concat r3)
          putStrLn err
        Right opts -> do
          let optStr = show $ concat opts
          -- process options here -- already parsed.  Load the file
          -- and change the EmitOptions appropriately.
          putStrLn optStr
          result <- P.parseFile fname
          case result of
            Left err -> do
              putStrLn ("Error: " ++ err)
              return ()
            Right (PR.Buffer emitopts frames) -> do
              let partialOpts = normalize $ concat opts
                  finalEmitOptions = (P.optionUpdate emitopts partialOpts)
                  layout = L.compileFrames x64Layout finalEmitOptions frames
              case layout of
                Left s -> putStrLn ("Error in compilation: " ++ s)
                Right layouts -> do
                  let files = CP.cppFiles finalEmitOptions layouts
                  forM_ files (\(fname, text) -> writeFile fname (PP.render text)) 
              return ()
          return ()
