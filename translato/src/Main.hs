{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Functor
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Text.XML as XML
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Environment
import System.FilePath

import LibContent hiding (libname)
import Reorganizer
import ShimConfig
import Shims
import UserAgentTools

import Debug.Trace

data Options = 
  Libo{libname::FilePath, userAgent::String} 
  | Translate{filename::FilePath, userAgent::String} 
  | Reorganize{userAgent::String} 
  deriving (Show, Data, Typeable)

liboOptions = 
  record Libo{} [
    libname := def += typ "FILENAME" += argPos 0,
    userAgent := def += typ "USERAGENT" += argPos 1
    ]

translateOptions = 
  record Translate{} [
    filename := def += typ "FILENAME" += argPos 0,
    userAgent := def += typ "USERAGENT" += argPos 1
    ]

reorganizeOptions = 
  record Reorganize{} [
    userAgent := def += typ "USERAGENT" += argPos 0
    ]

options = modes_ [liboOptions, translateOptions, reorganizeOptions]


--      += summary "Apply shims, reorganize, and generate to the input"


main::IO ()
main = do
  opts <- cmdArgs_ options
  result <- runEitherT $ doit opts
  case result of
    Left err -> error err
    Right value -> putStrLn value
    
-------------------

shimDir = "/home/jim/GlowApps/html5/shims"

bailIfNeeded::Show a=>Either a b->b
bailIfNeeded (Right x) = x
bailIfNeeded (Left err) = error $ show err

doit::Options->EitherT String IO String


doit Translate{filename=filename, userAgent=userAgent} = do
  input <- liftIO $
    case filename of
         "-" -> getContents
         _ -> readFile filename
  
  let specName =
        case takeExtension filename of
          ('.':ext) -> ext
          _ -> T.unpack $ tagNameToSpecName $ XML.nameLocalName $ XML.elementName $ XML.documentRoot $ bailIfNeeded $ XML.parseLBS XML.def (B.pack input)
    
  applyShims shimDir specName (getRealUserAgent userAgent) input

  where
    tagNameToSpecName::T.Text->T.Text
    tagNameToSpecName x = case T.stripSuffix "File" x of
      Just result -> trace "noerror" $ result
      Nothing -> error ("Unknown root element: " ++ T.unpack x)

doit Libo{libname=libname, userAgent=userAgent} = do

  maybeContent <- liftIO $ libContent shimDir (getRealUserAgent userAgent) $ name2Lib libname
  case maybeContent of
    Just output -> right output
    _ -> left "error"

doit Reorganize{userAgent=userAgent} = do
  input <- liftIO $ TL.getContents
  shimConfigs <- liftIO $ getAllShimConfigs shimDir
  reorganize shimConfigs (getRealUserAgent userAgent) input
