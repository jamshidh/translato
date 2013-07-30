-----------------------------------------------------------------------------
--
-- Module      :  EStringTools
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

module EStringTools (
    cleanDraw,
    cleanDrawForest,
    fillInFutureItems,
    fillInVariableAssignments,
    fillInAttributes
) where

import Data.Functor
import Data.List
import Data.Tree

import EnhancedString
import TreeTools

import JDebug

{--cleanTree::Tree EChar->Tree EString
cleanTree (Node {rootLabel=c, subForest=[next]})=Node { rootLabel=c:rootLabel nextResult, subForest=subForest nextResult }
    where nextResult = cleanTree next
cleanTree (Node {rootLabel=c, subForest=subForest}) = Node {rootLabel=[c], subForest=cleanTree <$> subForest}--}

cleanDraw::Tree EChar->String
cleanDraw = safeDrawTree . (fmap ((intercalate ", ") . cleanEString)) . cleanTree

cleanDrawForest::Forest EChar->String
cleanDrawForest forest = intercalate "\n" (map cleanDraw forest)

isChar::EChar->Bool
isChar (Ch _) = True
isChar _ = False

cleanEString::EString->[String]
cleanEString input@(Ch c:rest) = show (map (\(Ch c) -> c) chars):cleanEString rest
    where (chars, rest) = span isChar input
cleanEString (first:rest) = show first:cleanEString rest
cleanEString [] = []

fillInFutureItems::EString->EString
fillInFutureItems (FutureItem:rest) = futureItem ++ fillInFutureItems restWithoutFutureItem
    where
        (futureItem, restWithoutFutureItem) = splitFutureItem 0 rest
        splitFutureItem 0 (ItemInfo eString:rest) = (eString, rest)
        splitFutureItem count (c@(ItemInfo eString):rest) = fmap (c:) (splitFutureItem (count-1) rest)
        splitFutureItem count (c@FutureItem:rest) = fmap (c:) (splitFutureItem (count+1) rest)
        splitFutureItem count (c:rest) = fmap (c:) (splitFutureItem count rest)
        splitFutureItem _ x = error ("Missing case in splitFutureItem: " ++ show x)
fillInFutureItems (c:rest) = c:fillInFutureItems rest
fillInFutureItems [] = []

fillInVariableAssignments::EString->EString
fillInVariableAssignments (VStart name:rest) = VAssign name value:fillInVariableAssignments restWithoutVariableValue
    where
        (value, restWithoutVariableValue) = splitVariableValue rest
        splitVariableValue (c@VEnd:rest) = ("", rest)
        splitVariableValue (Ch c:rest) = (c:value, rest2)
            where (value, rest2) = splitVariableValue rest
        splitVariableValue (EStart _ _:rest) = splitVariableValue rest
        splitVariableValue (EEnd _:rest) = splitVariableValue rest
        splitVariableValue x = error ("Missing case in splitVariableValue: " ++ show x)
fillInVariableAssignments (c:rest) = c:fillInVariableAssignments rest
fillInVariableAssignments [] = []

fillInAttributes::EString->EString
fillInAttributes (EStart name atts:rest) =
    FilledInEStart name attributesWithValues:fillInAttributes restWithoutValues
    where
        (attributesWithValues, restWithoutValues) = splitAtts 0 rest atts
        splitAtts::Int->EString->[String]->([(String, String)], EString)
        splitAtts _ rest [] = ([], rest)
        splitAtts 0 (VAssign name value:rest) neededAtts | name `elem` neededAtts = ((name, value):atts, rest2)
            where (atts, rest2) = splitAtts 0 rest (filter (/= name) neededAtts)
--        splitAtts 0 (VAssign name value:rest) neededAtts = error "attribute value not needed"
        splitAtts 0 (c@(EEnd _):rest) neededAtts = ([], c:rest)
        splitAtts count (c@(EEnd _):rest) neededAtts = fmap (c:) (splitAtts (count-1) rest neededAtts)
        splitAtts count (c@(EStart _ _):rest) neededAtts = fmap (c:) (splitAtts (count+1) rest neededAtts)
        splitAtts count (c:rest) neededAtts = fmap (c:) (splitAtts count rest neededAtts)
fillInAttributes (c:rest) = c:fillInAttributes rest
fillInAttributes [] = []
