{-# LANGUAGE OverloadedStrings #-}

module Reorganizer (
  reorganize
  ) where

import Data.Default
import Data.Either
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Directory.Tree
import Text.XML
import Text.XML.Cursor

import LibContent
import Shims

--import Debug.Trace

reorganize::FilePath->String->TL.Text->IO (Either String TL.Text)
reorganize shimDir userAgent input = do
  case parseText def input of
    Right doc -> Right <$> renderText def <$> modify shimDir userAgent doc
    Left err -> return $ Left $ ("Error parsing document: " ++ show err)

--Gets the needed libs in one file, and then recursively get all the needed libs in each needed lib
{-addRecursivelyNeededLibs::DirTree TL.Text->Name->Document->S.Set T.Text->S.Set T.Text
addRecursivelyNeededLibs shimDir libTagName doc currentLibs = trace "abcd" $
  let immediateLibs = S.union currentLibs $ getNeededLibs libTagName doc in
  immediateLibs

addLibLibs::DirTree TL.Text->Lib->S.Set Lib->S.Set Lib
addLibLibs _ libname currentLibs | libname `S.member` currentLibs = currentLibs
addLibLibs shimDir libname currentLibs = 
  S.union (S.insert libname currentLibs) (addLibLibs shimDir libname =<< libLibs libname)
  where
    libLibs::Lib->S.Set Lib
    libLibs lib = documentToLibs $ libToDocument lib-}

libToDocument::FilePath->String->Lib->IO Document
libToDocument shimDir userAgent lib = do
  maybeContent <- libContent shimDir userAgent lib 
  case maybeContent of
    Just content -> parseText_ def <$> TL.pack <$> applyShims shimDir (libToParserSpec lib) userAgent content
    Nothing -> error ("error calling libContent for " ++ show lib)

libToParserSpec::Lib->String
libToParserSpec (JSLib _) = "js"
libToParserSpec (CSSLib _) = "css"
    
--Gets needed libs in a single file, but don't add libs needed by those libs
documentToLibs::FilePath->String->S.Set Lib->Document->IO (S.Set Lib)
documentToLibs shimDir userAgent currentLibs doc = do 
  let newLibs = libsDirectlyInDocument S.\\ currentLibs
  subLibs <- sequence $ ((documentToLibs shimDir userAgent (S.union currentLibs newLibs)  =<<) . (libToDocument shimDir userAgent)) <$> S.toList newLibs
  
  return $ S.union libsDirectlyInDocument (S.unions subLibs)
  
  where 
    libsDirectlyInDocument::S.Set Lib
    libsDirectlyInDocument = S.fromList $
      (JSLib <$> ((descendant $ fromDocument doc) >>= element "library" >>= attribute "src"))
      ++ (CSSLib <$> ((descendant $ fromDocument doc) >>= element "styleLib" >>= attribute "src"))
      
  


modify::FilePath->String->Document->IO Document
modify shimDir userAgent doc = do
  libs <- documentToLibs shimDir userAgent S.empty doc
  return $ modifyRoot (addLibs (S.toList libs)) doc
  where
    modifyRoot::(Element->[Element])->Document->Document
    modifyRoot f (Document prologue root epilogue) = 
      case f root of
           [uniqueRoot] -> Document prologue uniqueRoot epilogue

libname2El::Lib->Element
libname2El (JSLib libname) = Element "script" (M.fromList [("src", "/lib/" `T.append` libname)]) []
libname2El (CSSLib libname) = Element "link" (M.fromList [("rel", "stylesheet"), ("type", "text/css"), ("href", "/lib/" `T.append` libname)]) []
                                                     
addLibs::[Lib]->Element->[Element]
addLibs libs (Element "library" attrs children) = []
addLibs libs (Element "styleLib" attrs children) = []
addLibs libs (Element "head" attrs children) = 
  [Element "head" attrs (
      (NodeElement <$> libname2El <$> libs) 
      ++ (passTo (addLibs libs) =<< children))]
addLibs libs (Element name attrs children) = [Element name attrs (passTo (addLibs libs) =<< children)]

passTo::(Element->[Element])->Node->[Node]
passTo f (NodeElement el) = NodeElement <$> f el
passTo _ (NodeContent text) = [NodeContent text]
