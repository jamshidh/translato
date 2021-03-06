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
    cleanUpAfterError,
    expandOperators
) where

import Prelude hiding (lookup)

import Data.Functor
import Data.List hiding (lookup, insert)
import Data.Int
import qualified Data.Map as M
import Data.Tree
import qualified Data.Text.Lazy as TL

import EnhancedString
import Format
import qualified LString as LS
import ParseError
import TreeTools

--import JDebug

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

cleanUpAfterError::EString->EString
cleanUpAfterError s = cleanUpAfterError' s []
    where
        cleanUpAfterError'::EString->EString->EString
        cleanUpAfterError' [] [] = []
        cleanUpAfterError' [] state =
            error ("Error in cleanUpAgterError: string ended without closing all tags or ending values" ++ show state)
        cleanUpAfterError' (c@StartBlock:rest) needed =
            c:cleanUpAfterError' rest (EndBlock:needed)
        cleanUpAfterError' (c@EndBlock:rest) (EndBlock:neededRest) =
            c:cleanUpAfterError' rest neededRest
        cleanUpAfterError' (c@(EStart name _):rest) needed =
            c:cleanUpAfterError' rest (EEnd name:needed)
        cleanUpAfterError' (c@(EEnd name):rest) (EEnd _:neededRest) =
            c:cleanUpAfterError' rest neededRest
        cleanUpAfterError' (c@(VStart name _):rest) needed =
            c:cleanUpAfterError' rest (VEnd:needed)
        cleanUpAfterError' (c@VEnd:rest) (VEnd:neededRest) =
            c:cleanUpAfterError' rest neededRest
        cleanUpAfterError' [c@(Fail _)] needed = c:needed
        cleanUpAfterError' (c:rest) needed = c:cleanUpAfterError' rest needed
        --cleanUpAfterError' x _ = error ("Missing case in cleanUpAfterError: " ++ show x)


checkForVarConsistency::[M.Map TL.Text (Maybe TL.Text, LS.LString)]->EString->EString
checkForVarConsistency vStack (e@(EStart _ _):rest) = e:checkForVarConsistency (M.empty:vStack) rest
checkForVarConsistency (_:vStackRest) (e@(EEnd _):rest) = e:checkForVarConsistency vStackRest rest
checkForVarConsistency (vars:vStackRest) (e@(VAssign name val s):rest) =
    case M.lookup name vars of
        Nothing -> e:checkForVarConsistency (M.insert name (val, s) vars:vStackRest) rest
        Just (val2, s2) ->
            (if val == val2
                then []
                else [Fail $ MatchError
                                (TL.unpack name)
                                [rangeAt s2 (maybeLength val2), rangeAt s (maybeLength val)]
                                (formatMaybe val2)
                                (formatMaybe val)
                                ] ++ checkForVarConsistency (vars:vStackRest) rest)
            ++ checkForVarConsistency (vars:vStackRest) rest
    where
        maybeLength::Maybe TL.Text->Int64
        maybeLength (Just val) = TL.length val
        maybeLength Nothing = 0
checkForVarConsistency vStack (c:rest) = c:checkForVarConsistency vStack rest
checkForVarConsistency _ [] = []

fillInVariableAssignments::EString->EString
fillInVariableAssignments (VStart name Nothing:rest) = error ("fillInVariableAssignments called without LString for '" ++ TL.unpack name ++ "'")
fillInVariableAssignments (VStart name (Just s):rest) =
    VAssign name value s:fillInVariableAssignments (errors ++ restWithoutVariableValue) --I extract the value, and place any errors after the location of the extraction.
    where
        (value, restWithoutVariableValue, errors) = splitVariableValue rest
        splitVariableValue::EString->(Maybe TL.Text, EString, EString)
        splitVariableValue (c@VEnd:rest) = (Just TL.empty, rest, [])
        splitVariableValue (Ch c:rest) =
            case splitVariableValue rest of
                (Just value, rest2, errors2) -> (Just (TL.cons c value), rest2, errors2)
                (Nothing, rest2, errors2) -> (Nothing, rest2, errors2)
        splitVariableValue (EStart _ _:rest) = splitVariableValue rest
        splitVariableValue (EEnd _:rest) = splitVariableValue rest
        splitVariableValue (Fail err:rest) = 
          (valueRest, remainder, Fail err:errorsRest)
          where
            (valueRest, remainder, errorsRest) = splitVariableValue rest
        splitVariableValue x = error ("Missing case in splitVariableValue: " ++ (format =<< x))
fillInVariableAssignments (c:rest) = c:fillInVariableAssignments rest
fillInVariableAssignments [] = []

fillInAttributes::EString->EString
fillInAttributes (EStart name atts:rest) = 
    FilledInEStart name attributesWithValues:fillInAttributes restWithoutValues
    where
        (attributesWithValues, restWithoutValues) = splitAtts rest $ M.toList atts

        splitAtts::EString->[(TL.Text, Bool)]->([(TL.Text, Maybe TL.Text)], EString)
        splitAtts (VAssign name value _:rest) neededAtts | name `elem` (fst <$> neededAtts) =
             ((name, value):atts, rest2)
            where (atts, rest2) = splitAtts rest (filter ((/= name) . fst) neededAtts)
        splitAtts (VAssign name value _:rest) neededAtts = error ("attribute " ++ show name ++ " not needed: just need " ++ show neededAtts)
        splitAtts (c@(EEnd _):rest) neededAtts | null $ filter (not . snd) neededAtts = ([], c:rest)
        splitAtts rest [] = ([], rest)
        splitAtts (EEnd _:_) neededAtts = error ("Error in splitAtts: missing attributes '" ++ show neededAtts ++ "'")
        splitAtts (e@(EStart _ _):rest) neededAtts =
	  (e:) <$> (inside ++) <$> splitAtts after neededAtts
	       where (after, inside) = itemsAfterEEnd rest
        splitAtts (c:rest) neededAtts = fmap (c:) (splitAtts rest neededAtts)
        splitAtts sq neededAtts =
            error ("Missing case in splitAtts: sq = " ++ show sq ++ ", neededAtts = " ++ show neededAtts)

        itemsAfterEEnd::EString->(EString, EString)
        itemsAfterEEnd [] = error ("Error in itemsAfterEEnd: hit end of estring without an EEnd")
        itemsAfterEEnd (e@(EEnd _):rest) = (e:rest, [])
        itemsAfterEEnd (e@(EStart _ _):rest) = (e:) <$> (inside ++) <$> itemsAfterEEnd after
	    where (after, inside) = itemsAfterEEnd rest
        itemsAfterEEnd (c:rest) = (c:) <$> itemsAfterEEnd rest



fillInAttributes (c:rest) = c:fillInAttributes rest
fillInAttributes [] = []

fstMap f (first, second) = (f first, second)

expandOperators::EString->EString
expandOperators (StartBlock:rest) = case fullBlock rest of
    Right (blockString, rest2) -> expandOperatorsInBlock blockString ++ expandOperators rest2
    Left e -> [e]
    where
        fullBlock::EString->Either EChar (EString, EString)
        
        --fullBlock [] = error "Fullblock hit the end without an EndBlock" -- This can happen if an earlier processed error in the block cuts off the endblock
        fullBlock [] = Right ([], rest) -- Therefore I'm doing this for now, although this doesn't feel quite kosher
        
        fullBlock (EndBlock:rest) = Right ([], rest)
--        fullBlock (c@(Fail _):rest) = Left c
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

--TODO- I think there is a bug when there are parentheses.  We remove all the block start/end infoBarAddActionWidget
--      right off the bat, including that in parens, but this might be needed for parens.
--      I am not sure though, and don't want to deal with this now.
expandOperators (c:rest) = c:expandOperators rest
expandOperators [] = []

expandOperatorsInBlock::EString->EString
expandOperatorsInBlock (FilledInEStart name atts:rest) =
    expandOperatorsInBlock (NestedItem (FilledInEStart name atts:expandedBlockString ++ [endTag]):rest2)
    where
        (blockString, endTag, rest2) = --jtrace ("endtag: " ++ show endTag) $ 
		      getNestedItem rest
        expandedBlockString = expandOperatorsInBlock blockString
expandOperatorsInBlock (n@(NestedItem _):t@(InfixTag _):FilledInEStart name atts:rest) =
    expandOperatorsInBlock (n:t:NestedItem (FilledInEStart name atts:expandedBlockString ++ [endTag]):rest2)
    where
        (blockString, endTag, rest2) = getNestedItem rest
        expandedBlockString = expandOperatorsInBlock blockString

expandOperatorsInBlock (NestedItem item:InfixTag InfixOp{opName=name,opAssociativity=UseEndCap}:rest) =
    expandOperatorsInBlock
        ([FilledInEStart name []] ++ item ++ expandOperatorsInBlock inside ++ [EEnd name] ++ outside)
        where (inside, outside) = splitByEndCap rest


expandOperatorsInBlock (NestedItem left:InfixTag o1:NestedItem right:InfixTag o2:rest) =
    simplifyOpPair left o1 right o2 rest
expandOperatorsInBlock (NestedItem left:InfixTag InfixOp{opName=name}:NestedItem right:rest) =
    [FilledInEStart name []] ++ left ++ right ++ [EEnd name] ++ expandOperatorsInBlock rest
expandOperatorsInBlock [NestedItem item] = item
expandOperatorsInBlock (NestedItem left:e@(FilledInEStart _ _):rest) =
    left ++ expandOperatorsInBlock (e:rest)
expandOperatorsInBlock [NestedItem item, Fail err] = item ++ [Fail err]
expandOperatorsInBlock items | null $ filter (not . isCharacter) items = items
  where
    isCharacter (Ch _) = True
    isCharacter _ = False
expandOperatorsInBlock [] = []
expandOperatorsInBlock s = s --Only needed for error reporting....  Note, this *will* pass through inline operators and nested items that should not be passed through
  --error ("Missing case in expandOperatorsInBlock: (" ++ (format =<< s))


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

simplifyOpPair _ op1 _ op2 _ | opPriority op1 == opPriority op2 = error ("Error in simplifyOpPair: Ops have same priority: " ++ show op1 ++ " and " ++ show op2)

simplifyOpPair left op1 right op2 _ = error ("Missing case in simplifyOpPair: " ++ show op1 ++ ", " ++ show op2)


splitByEndCap::EString->(EString, EString)
splitByEndCap [] = ([], []) --This is a hack to get things working in a case where an error has gutten the output.
splitByEndCap (EndCap name:rest) = ([], rest)
splitByEndCap (e@(InfixTag InfixOp{opAssociativity=UseEndCap, opName=name}):rest) = (e:inside1 ++ [EndCap name] ++ inside2, outside2)
    where
        (inside1, outside1) = splitByEndCap rest
        (inside2, outside2) = splitByEndCap outside1
splitByEndCap (c:rest) = (c:inside, outside)
    where (inside, outside) = splitByEndCap rest
splitByEndCap x = error ("Missing case in splitByEndCap: " ++ (format =<< x))



getNestedItem::EString->(EString, EChar, EString)
getNestedItem (EEnd name:rest) = ([], EEnd name, rest)
getNestedItem (FilledInEStart name atts:rest) = ([FilledInEStart name atts] ++ inside1 ++ endTag1:inside2, endTag2, outside2)
    where
        (inside1, endTag1, outside1) = getNestedItem rest
        (inside2, endTag2, outside2) = getNestedItem outside1
getNestedItem (c:rest) = (c:inside, endTag, outside)
    where (inside, endTag, outside) = getNestedItem rest
getNestedItem x = error ("Missing case in getNestedItem '" ++ show x ++ "'")


