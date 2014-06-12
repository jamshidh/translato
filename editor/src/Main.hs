{-# OPTIONS_GHC -Wall #-}

module Main (
    main
) where

import Data.Functor
import qualified Data.Map as M
import System.Environment

import Editor

--import JDebug

main::IO()
main = do
    args <- getArgs
    editMain args







