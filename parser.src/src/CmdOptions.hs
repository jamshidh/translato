{-# Language TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  CmdOptions
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module CmdOptions (
arg2Opts
) where

import Data.List

import StupidSetGet

$([d|

    usage current = error ("Usage:\n" ++ unlines (getFields current))

    arg2Opts::Show a=>Read a=>[String]->a->a
    arg2Opts ("--help":_) current = usage current
    arg2Opts (('-':'-':optField):value:rest) current | not $ isPrefixOf "--" value = arg2Opts rest newOpts
        where newOpts = setFromString optField value current
    arg2Opts (('-':'-':'n':'o':'-':optField):rest) current = arg2Opts rest newOpts
        where newOpts = setFromString optField "False" current
    arg2Opts (('-':'-':optField):rest) current = arg2Opts rest newOpts
        where newOpts = setFromString optField "True" current
    arg2Opts (x:_) _ = error ("Unknown option: " ++ show x)
    arg2Opts [] current = current

    |])

data OptType = OptString | OptInt

data ParsedOpt = ParsedOpt { name::String, shortName::Char, optType::OptType }

parseOptions::Show a=>a->[ParsedOpt]
parseOptions input = []
    where inputString = show input
