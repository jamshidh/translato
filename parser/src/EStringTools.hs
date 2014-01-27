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
    checkForVarConsistency,
    fillInVariableAssignments,
    fillInAttributes,
    expandOperators
) where

import Prelude hiding (lookup)

import Data.Functor
import Data.List hiding (lookup, insert)
import Data.Map hiding (map, filter)
import Data.Tree

import EnhancedString
import Format
import qualified LString as LS
import ParseError
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

addLocationString::LS.LString->EString->EString
addLocationString _ [] = []
addLocationString ls (VStart name _:rest) = VStart name (Just ls):addLocationString ls rest
addLocationString ls (c:rest) = c:addLocationString ls rest

fillInFutureItems::EString->EString
fillInFutureItems (FutureItem (Just ls):rest) = addLocationString ls futureItem ++ fillInFutureItems restWithoutFutureItem
    where
        (futureItem, restWithoutFutureItem) = splitFutureItem rest
        splitFutureItem seq@(FutureItem s2:rest) = splitFutureItem $ fillInFutureItems seq
        splitFutureItem (ItemInfo eString:rest) = (eString, rest)
        splitFutureItem seq@(Fail _:rest) = ([Unknown], seq)
        splitFutureItem (c:rest) = fmap (c:) (splitFutureItem rest)
        splitFutureItem x = error ("Missing case in splitFutureItem: " ++ show x)
fillInFutureItems (c:rest) = c:fillInFutureItems rest
fillInFutureItems [] = []

checkForVarConsistency::[Map String (Maybe String, LS.LString)]->EString->EString
checkForVarConsistency vStack (e@(EStart _ _):rest) = e:checkForVarConsistency (empty:vStack) rest
checkForVarConsistency (_:vStackRest) (e@(EEnd _):rest) = e:checkForVarConsistency vStackRest rest
checkForVarConsistency (vars:vStackRest) (e@(VAssign name val s):rest) =
    case lookup name vars of
        Nothing -> e:checkForVarConsistency (insert name (val, s) vars:vStackRest) rest
        Just (val2, s2) ->
            (if val == val2
                then []
                else [Fail $ MatchError
                                name
                                [rangeAt s2 (maybeLength val2), rangeAt s (maybeLength val)]
                                (formatMaybe val2)
                                (formatMaybe val)
                                ] ++ checkForVarConsistency (vars:vStackRest) rest)
            ++ checkForVarConsistency (vars:vStackRest) rest
    where
        maybeLength::Maybe String->Int
        maybeLength (Just val) = length val
        maybeLength Nothing = 0
checkForVarConsistency vStack (c:rest) = c:checkForVarConsistency vStack rest
checkForVarConsistency _ [] = []

fillInVariableAssignments::EString->EString
fillInVariableAssignments (VStart name Nothing:rest) = error ("fillInVariableAssignments called without LString for '" ++ name ++ "'")
fillInVariableAssignments (VStart name (Just s):rest) =
    VAssign name value s:fillInVariableAssignments restWithoutVariableValue
    where
        (value, restWithoutVariableValue) = splitVariableValue rest
        splitVariableValue::EString->(Maybe String, EString)
        splitVariableValue (c@VEnd:rest) = (Just "", rest)
        splitVariableValue (Ch c:rest) =
            case splitVariableValue rest of
                (Just value, rest2) -> (Just (c:value), rest2)
                (Nothing, _) -> (Nothing, [])
        splitVariableValue (Fail err:rest) = (Nothing, [Fail err])
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
        splitAtts::Int->EString->[String]->([(String, Maybe String)], EString)
        splitAtts _ rest [] = ([], rest)
        splitAtts 0 (VAssign name value _:rest) neededAtts | name `elem` neededAtts = ((name, value):atts, rest2)
            where (atts, rest2) = splitAtts 0 rest (filter (/= name) neededAtts)
--        splitAtts 0 (VAssign name value:rest) neededAtts = error "attribute value not needed"
        splitAtts 0 (c@(EEnd _):rest) neededAtts = ([], c:rest)
        splitAtts count (c@(EEnd _):rest) neededAtts = fmap (c:) (splitAtts (count-1) rest neededAtts)
        splitAtts count (c@(EStart _ _):rest) neededAtts = fmap (c:) (splitAtts (count+1) rest neededAtts)
        splitAtts count (c@(Fail _):rest) neededAtts =
            ((\x -> (x, Nothing)) <$> neededAtts, [c])
        splitAtts count (c:rest) neededAtts = fmap (c:) (splitAtts count rest neededAtts)
        splitAtts count s neededAtts = error ("Missing case in splitAtts: " ++ show count ++ ", " ++ show s ++ ", " ++ show neededAtts)
fillInAttributes (c:rest) = c:fillInAttributes rest
fillInAttributes [] = []

fstMap f (first, second) = (f first, second)

expandOperators::EString->EString
expandOperators (StartBlock:rest) = case fullBlock rest of
    Right (blockString, rest2) -> expandOperatorsInBlock blockString ++ expandOperators rest2
    Left e -> [e]
    where
        fullBlock::EString->Either EChar (EString, EString)
        fullBlock [] = error "Fullblock hit the end without an EndBlock"
        fullBlock (EndBlock:rest) = Right ([], rest)
        fullBlock (c@(Fail _):rest) = Left c
        fullBlock (StartBlock:rest) = --Nested Blocks
            case fullBlock rest of
                Right (inside1, outside1) ->
                    case fullBlock outside1 of
                        Right (inside2, outside2) -> Right (inside1 ++ inside2, outside2)
                        Left e -> Left e
                Left e -> Left e
        fullBlock (c:rest) = case fullBlock rest of
                Right (inside, outside) -> Right (c:inside, outside)
                Left e -> Left e

expandOperators (c:rest) = c:expandOperators rest
expandOperators [] = []

expandOperatorsInBlock::EString->EString
expandOperatorsInBlock (FilledInEStart name atts:rest) =
    expandOperatorsInBlock (NestedItem (FilledInEStart name atts:blockString):rest2)
    where
        (blockString, rest2) = getNestedItem rest
expandOperatorsInBlock (n@(NestedItem _):t@(InfixTag _):FilledInEStart name atts:rest) =
    expandOperatorsInBlock (n:t:NestedItem (FilledInEStart name atts:blockString):rest2)
    where
        (blockString, rest2) = getNestedItem rest

expandOperatorsInBlock (NestedItem item:InfixTag InfixOp{opName=name,opAssociativity=UseEndCap}:rest) =
    expandOperatorsInBlock
        ([FilledInEStart name []] ++ item ++ expandOperatorsInBlock inside ++ [EEnd name] ++ outside)
        where (inside, outside) = splitByEndCap rest


expandOperatorsInBlock
    (NestedItem left:InfixTag o1:NestedItem right:InfixTag o2:rest)
    = simplifyOpPair left o1 right o2 rest
expandOperatorsInBlock (NestedItem left:InfixTag InfixOp{opName=name}:NestedItem right:rest) =
    [FilledInEStart name []] ++ left ++ right ++ [EEnd name] ++ expandOperatorsInBlock rest
expandOperatorsInBlock (NestedItem left:e@(FilledInEStart _ _):rest) =
    left ++ expandOperatorsInBlock (e:rest)
expandOperatorsInBlock [NestedItem item] = item
expandOperatorsInBlock [] = []
expandOperatorsInBlock s =
    error ("Missing case in expandOperatorsInBlock: (" ++ show s)


simplifyOpPair::EString->InfixOp->EString->InfixOp->EString->EString
{--simplifyOpPair item1 op1 item2 op2@InfixOp{opAssociativity=InfixUseEndCap} rest =
    expandOperatorsInBlock
        (NestedItem item1:InfixTag op1:expandOperatorsInBlock (NestedItem item2:InfixTag op2:rest))--}

simplifyOpPair left InfixOp{opName=name,opPriority=p1} right op2 rest
    | p1 < opPriority op2 =
    expandOperatorsInBlock
        (NestedItem ([FilledInEStart name []] ++ left ++ right ++ [EEnd name]):InfixTag op2:rest)

simplifyOpPair left op1 right op2@InfixOp{opPriority=p2} rest
    | opPriority op1 > p2 =
    expandOperatorsInBlock
        (left ++ [InfixTag op1] ++ expandOperatorsInBlock (NestedItem right:InfixTag op2:rest))

simplifyOpPair left op1@InfixOp{opName=name,opAssociativity=LeftAssoc,opPriority=p1} right op2 rest
    | op1 == op2 =
    expandOperatorsInBlock (NestedItem ([FilledInEStart name []] ++ left ++ right ++ [EEnd name]):InfixTag op2:rest)

simplifyOpPair left op1@InfixOp{opName=name,opPriority=p1} right op2 rest
    | op1 == op2 =
    expandOperatorsInBlock
        (left ++ [InfixTag op1] ++ expandOperatorsInBlock (NestedItem right:InfixTag op2:rest))

splitByEndCap::EString->(EString, EString)
splitByEndCap (EndCap name:rest) = ([], rest)
splitByEndCap (e@(InfixTag InfixOp{opAssociativity=UseEndCap, opName=name}):rest) = (e:inside1 ++ [EndCap name] ++ inside2, outside2)
    where
        (inside1, outside1) = splitByEndCap rest
        (inside2, outside2) = splitByEndCap outside1
splitByEndCap (c:rest) = (c:inside, outside)
    where (inside, outside) = splitByEndCap rest


getNestedItem::EString->(EString, EString)
getNestedItem (EEnd name:rest) = ([EEnd name], rest)
getNestedItem (FilledInEStart name atts:rest) = ([FilledInEStart name atts] ++ inside1 ++ inside2, outside2)
    where
        (inside1, outside1) = getNestedItem rest
        (inside2, outside2) = getNestedItem outside1
getNestedItem (c:rest) = (c:inside, outside)
    where (inside, outside) = getNestedItem rest


