{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# OPTIONS_GHC -Wall #-}

module OutputParser (
  pparseMain
) where

import Control.Monad
import Data.Functor
import Data.List
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.FilePath
import System.IO

import FieldMarshal

import ArgOpts
import Parser


data Options = Options { specName::Maybe String, inputFileName::String }

$(deriveFieldMarshal ''Options ''String)

deflt::Options
deflt = Options { specName = Nothing, inputFileName="-" }

pparseMain::[String]->IO ()
pparseMain args = do
    let options = args2Opts args ["inputFileName"] deflt

    let theSpecName =
          case msum [specName options, getFileExtension $ inputFileName options] of
            Just x -> x
            _ -> error "You need to supply 'specName' when the inputFileName doesn't have an extension"

    input <-
      case inputFileName options of
        "-" -> TL.getContents
        theFileName -> TL.hGetContents =<< openFile theFileName ReadMode

    putStrLn =<< prettify <$> parseUsingSpecName theSpecName input

getFileExtension::FilePath->Maybe String
getFileExtension x =
  case takeExtension x of
    ('.':ext) -> Just ext
    _ -> Nothing



prettify::String->String
prettify [] = []
prettify s | "<error>" `isPrefixOf` s = "\ESC[31m\n  <error>" ++ (showError $ drop 7 s)
prettify (c:rest) = c:prettify rest

showError::String->String
showError [] = []
showError ('&':'l':'t':';':rest) = '<':showError rest
showError ('&':'g':'t':';':rest) = '>':showError rest
showError ('&':'q':'u':'o':'t':';':rest) = '"':showError rest
showError ('&':'a':'p':'o':'s':';':rest) = '\'':showError rest
showError s | "</error>" `isPrefixOf` s = "\n  </error>\ESC[0m" ++ (prettify $ drop 8 s)
showError (c:rest) = c:showError rest

