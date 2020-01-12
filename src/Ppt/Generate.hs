{-# LANGUAGE OverloadedStrings #-}
module Ppt.Generate where

import qualified Ppt.Frame.Parser as P
import qualified Ppt.Frame.ParsedRep as PR
--import qualified Ppt.Frame.Layout as L
import Ppt.Frame.LayoutAlgo as LA

import System.IO
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
import qualified Ppt.Generate.CpConfig as CPC

import System.Console.GetOpt

import qualified Ppt.Frame.Util as U

{-
   Command line processing support
-}

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

-- Yup, hideous, needs help.
generateCommand :: [String] -> IO ()
generateCommand args =
  let opts = splitOptionGroups args
  -- The following should all be in a runEitherT $ do...
  in case opts of
    Nothing -> do
      putStrLn "ppt generate [options] <spec-file>"
      putStrLn "Options:"
      putStrLn "  --prefix FILE-PREFIX"
      putStrLn "  --tag KEY VALUE"
      putStrLn "  --option OPTION"
      return ()
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
          putStrLn (L.concatMap L.concat r3)
          putStrLn err
        Right theopts -> do
          let optStr = show $ concat theopts
          -- process options here -- already parsed.  Load the file
          -- and change the EmitOptions appropriately.
          -- putStrLn optStr
          result <- P.parseFile fname (normalize $ concat theopts)
          case result of
            Left err -> do
              putStrLn ("Error: " ++ err)
              return ()
            Right (PR.Buffer emitopts frames) -> do
              let partialOpts = normalize $ concat opts
              case LA.compileFrames' CPC.x64Layout frames of
                Left s -> putStrLn ("Error in compilation: " ++ s)
                Right layouts -> do
                  let files = CP.cppFiles emitopts layouts
                  U.showFrameLayouts layouts
                  -- forM_ layouts (\f -> putStrLn (show f))
                  forM_ files (\(fname, text) -> writeFile fname (PP.render text))
              return ()
          return ()
