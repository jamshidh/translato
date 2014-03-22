{-# LANGUAGE OverloadedStrings #-}

module Reorganizer (
  reorganize
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Default
import Data.Either
import Data.Functor
import Data.Graph
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Tree
import System.Directory
import System.FilePath
import qualified Text.XML as XML
import Text.XML.Cursor

import LibContent
import Shims
import ShimConfig

--import Debug.Trace

reorganize::[ShimConfig]->String->TL.Text->EitherT String IO String
reorganize shimConfigs userAgent input = do
  case XML.parseText def input of
    Right doc -> do
      doc <- liftIO $ XML.renderText def <$> modify shimConfigs userAgent doc
      right $ TL.unpack doc
    Left err -> left $ ("Error parsing document: " ++ show err)


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

getShimFiles::FilePath->String->IO [(String, String)]
getShimFiles shimDir filename = do
  dependencies <- filterM doesFileExist =<< map (</> filename) <$> map (shimDir </>) <$> getDirectoryContents shimDir
  sequence $ nameAndContent <$> dependencies
  where
    nameAndContent::FilePath->IO (String, String)
    nameAndContent filepath = do
      contents <- readFile filepath
      return (takeFileName $ takeDirectory filepath, contents)
 
addLibsRecursively::[ShimConfig]->[ShimName]->[Lib]
addLibsRecursively shimConfigs neededLibs = do
  let (dependGraph, v2k, k2v) = graphFromEdges $ 
                                ([], ShimName "#basename#", neededLibs)
                                :
                                ((\ShimConfig{libs=l, name=n, dependencies=d} ->(l, n, d)) <$> shimConfigs)
                
      (childDependGraph, v2k', k2v') = graphFromEdges (v2k <$> reachable dependGraph (fromJust $ k2v $ ShimName "#basename#"))
    
    in concat $ fst3 <$> v2k' <$> (flatten =<< scc childDependGraph)

  where 
    fst3 (x, _, _) = x

{-libToDocument::FilePath->String->Lib->IO XML.Document
libToDocument shimDir userAgent lib = do
  maybeContent <- libContent shimDir userAgent lib 
  case maybeContent of
    Just content -> XML.parseText_ def <$> TL.pack <$> applyShims shimDir (libToParserSpec lib) userAgent content
    Nothing -> error ("error calling libContent for " ++ show lib)-}

libToParserSpec::Lib->String
libToParserSpec (JSLib _) = "js"
libToParserSpec (CSSLib _) = "css"
    
--Gets needed libs in a single file, but don't add libs needed by those libs
documentToLibs::XML.Document->[ShimName]
documentToLibs doc = 
      (ShimName <$> ((descendant $ fromDocument doc) >>= element "shimLib" >>= getNameAttribute))
      where --All of these helper functions are for error checking.  We need to make sure that each shimLib tag has one and only one "@name" tag.
        getNameAttribute::Cursor->[T.Text]
        getNameAttribute c = node2Name $ node c 
        node2Name::XML.Node->[T.Text]
        node2Name (XML.NodeElement XML.Element{XML.elementAttributes=atts}) = [uniqueNameValue $ M.toList atts]
        uniqueNameValue::[(XML.Name, T.Text)]->T.Text
        uniqueNameValue [] = error "A <shimLib> tag is missing its 'name' attribute."
        uniqueNameValue [("name", value)] = value
        uniqueNameValue atts = error ("unexpected attributes in <shimLib>: [" ++ intercalate ", " (showAttribute <$> atts) ++ "]\n----Remember, only 'name' is allowed.")
        showAttribute::(XML.Name, T.Text)->String
        showAttribute (n, v)=T.unpack (XML.nameLocalName n) ++ "='" ++ T.unpack v ++ "'"


modify::[ShimConfig]->String->XML.Document->IO XML.Document
modify shimConfigs userAgent doc = do
  let neededLibs = addLibsRecursively shimConfigs $ documentToLibs doc
  return $ modifyRoot (addLibs neededLibs) doc
  where
    modifyRoot::(XML.Element->[XML.Element])->XML.Document->XML.Document
    modifyRoot f (XML.Document prologue root epilogue) = 
      case f root of
           [uniqueRoot] -> XML.Document prologue uniqueRoot epilogue

libname2El::Lib->XML.Element
libname2El (JSLib libname) = XML.Element "script" (M.fromList [("src", "/lib/" `T.append` libname)]) []
libname2El (CSSLib libname) = XML.Element "link" (M.fromList [("rel", "stylesheet"), ("type", "text/css"), ("href", "/lib/" `T.append` libname)]) []
                                                     
addLibs::[Lib]->XML.Element->[XML.Element]
addLibs libs (XML.Element "shimLib" attrs children) = []
addLibs libs (XML.Element "head" attrs children) =
  [XML.Element "head" attrs (
      (XML.NodeElement <$> libname2El <$> libs) 
      ++ (passTo (addLibs libs) =<< children))]
addLibs libs (XML.Element name attrs children) = [XML.Element name attrs (passTo (addLibs libs) =<< children)]

passTo::(XML.Element->[XML.Element])->XML.Node->[XML.Node]
passTo f (XML.NodeElement el) = XML.NodeElement <$> f el
passTo _ (XML.NodeContent text) = [XML.NodeContent text]
