-----------------------------------------------------------------------------
--
-- Module      :  VarAssignment
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

module VarAssignment (
    assignVariables
) where

import Data.Functor
import Data.Map
import Data.Tree

import EnhancedString
import LString hiding (empty, head, tail)
import XPath

import JDebug

data VContext = VContext {
    conditionStack::[Condition],
    currentVarName::Maybe String,
    currentVarVal::Maybe String,
    currentVarInput::Maybe LString,
    variableStack::[Map String String] }
rootVContext = VContext {
    conditionStack=[],
    currentVarName=Nothing,
    currentVarVal=Nothing,
    currentVarInput=Nothing,
    variableStack=[]
    }


assignVariables::Forest EChar->Forest EChar
assignVariables forest = forest >>= assignVariablesUsingVContext rootVContext

------

assignVariablesUsingVContext::VContext->Tree EChar->Forest EChar

assignVariablesUsingVContext vcx node@Node{rootLabel=EStart _ _, subForest=subForest} =
    [node {subForest=subForest >>= assignVariablesUsingVContext (
                vcx {
                        variableStack=empty:variableStack vcx
                    })}]

assignVariablesUsingVContext vcx node@Node{rootLabel=EEnd _, subForest=subForest} =
    [node{subForest=subForest >>= assignVariablesUsingVContext (
                vcx {
                        conditionStack=tail (conditionStack vcx),
                        variableStack=tail (variableStack vcx)
                    })}]

assignVariablesUsingVContext vcx (Node {rootLabel=VStart name input, subForest=subForest}) =
    subForest >>= assignVariablesUsingVContext (
        vcx {
                currentVarName=Just name,
                currentVarVal=Just "",
                currentVarInput=Just input
            })

assignVariablesUsingVContext
    vcx@VContext{
        variableStack=vars:vrest,
        conditionStack=condition:crest,
        currentVarName=Just name,
        currentVarVal=Just val,
        currentVarInput=Just input
        }
    Node{rootLabel=VEnd, subForest=subForest} =
    if eval nextVars condition /= Just CnFalse
        then
            [Node {rootLabel=VAssign name val,
                subForest=subForest >>= assignVariablesUsingVContext (
                            vcx {
                                    variableStack=nextVars:vrest,
                                    currentVarName=Nothing,
                                    currentVarVal=Nothing
                                })}]
        else [Node {rootLabel=Error ("Condition failed: " ++ show condition) input, subForest=[]}]
            where nextVars = insert name val vars

assignVariablesUsingVContext _ (Node {rootLabel=VEnd, subForest=subForest}) =
    error "VEnd was hit, but variable name or value is 'Nothing'"

assignVariablesUsingVContext vcx@VContext{currentVarVal=Just val} (Node {rootLabel=Ch c,subForest=subForest}) =
    subForest >>= assignVariablesUsingVContext vcx{currentVarVal=Just (val++[c])}

assignVariablesUsingVContext vcx@VContext{currentVarVal=Just val} node@Node{rootLabel=Sync _} =
    [node {subForest=subForest node >>= assignVariablesUsingVContext vcx}]

assignVariablesUsingVContext vcx@VContext{currentVarVal=Just val} node@Node{rootLabel=Error _ _, subForest=[]} =
    [node]

assignVariablesUsingVContext vcx@VContext{currentVarVal=Just val} node@Node{rootLabel=ExpectationError _ _, subForest=[]} =
    [node]

assignVariablesUsingVContext vcx@VContext{currentVarVal=Just val} node =
    error ("Only characters are allowed in attribute: " ++ show (rootLabel node))

assignVariablesUsingVContext vcx (Node {rootLabel=rootLabel,subForest=subForest}) =
    [Node {rootLabel=rootLabel, subForest=subForest >>= assignVariablesUsingVContext vcx}]



