-----------------------------------------------------------------------------
--
-- Module      :  Generator
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

module Generator (
    generate,
    GenError (GenError)
) where

import Prelude hiding (lookup)
import Data.Text hiding (map, concat, foldl1, foldl, head, intercalate, tail)
import Data.Map hiding (map, foldl)
import Data.Maybe
import Data.List hiding (lookup)
import Text.XML
import Text.XML.Cursor
import Control.Arrow

import Debug.Trace

import OperatorNames
import GrammarParser
import GrammarTools
import EnhancedString

import Colors

------------------------------

name2String::Name->String
name2String name = unpack $ nameLocalName name

tagName::Cursor->String
--tagName (Cursor (NodeElement element)) = nameLocalName $ elementName element
tagName c = case node c of
    NodeElement element -> name2String $ elementName element

showCursor::Cursor->String
showCursor c = case node c of
    NodeElement element -> "<" ++ tagName c ++ " " ++ (intercalate " " (map (\(name, text) -> name2String name ++ "=\"" ++ unpack text ++ "\"") (elementAttributes element))) ++ ">"
    NodeContent text -> "Text:'" ++ unpack text

-------------------------------

data GenError = GenError String deriving (Show)

operatorRules::Grammar->Map String Expression
operatorRules g = fromList $ map (\op -> (op2Name op, SepBy2 [TextMatch ""] [TextMatch op])) (nub $ concat $ elems $ operatorDefinitions g)

expressionWithRulename::Grammar->RuleName->[Sequence]
expressionWithRulename g name = case lookup name (fromListWith (++) (map (\(a, b)-> (a, [b])) (elementRules g))) of
    Just x -> x
    Nothing -> case lookup name (operatorRules g) of
                    Just x -> [[x]]
                    Nothing -> []



{--table2Expression::[OperatorSymbol]->Expression->Expression
table2Expression [] terminal = terminal
table2Expression (symbol:rest) terminal = Sequence [ NestedElement (op2Name symbol) (SepBy (table2Expression rest terminal) (TextMatch symbol))]--}







generate::Grammar->Cursor->Either GenError String
generate g c = trace (show gr) $ fmap enhancedString2String (cursor2String gr c)
    where gr = (fullySimplifyGrammar $ expandOperators $ stripWhitespaceFromGrammar $ fullySimplifyGrammar g)

cursor2String::Grammar->Cursor->Either GenError String
cursor2String g c = trace ("----------cursor2String called: " ++ tagName c) $
    if (expressionList == []) then error ("Link '" ++ tagName c ++ "' doesn't exist in the grammar")
        else case cursorAndExpressions2String g expressionList c (child c) of
--            Right (s, _) -> Right ("\t" ++ s ++ "\b")
            Right (s, _) -> Right s
            Left e -> Left e
    where expressionList = (expressionWithRulename g (tagName c))


cursorAndExpressions2String::Grammar->[Sequence]->Cursor->[Cursor]->Either GenError (String, Maybe Cursor)
cursorAndExpressions2String g (e:rest) c remainingChildren =
    case seq2String g e c remainingChildren of
        ret@(Right (s, [])) -> (trace "Element match succeeded") $ Right (s, next c)
        ret@(Right (s, _)) -> (trace (red "Element match failed")) $
            if (rest == []) then
                    Left $ GenError ("There was some extra stuff in an element: In " ++ showCursor c ++ " is\n    " ++
                                        (intercalate " ---- " (map showCursor remainingChildren)))
                else cursorAndExpressions2String g rest c remainingChildren
        Left e -> if (rest == []) then Left e
                else cursorAndExpressions2String g rest c remainingChildren
cursorAndExpressions2String g [] c remainingChildren = error "Shouldn't be here"

(+++)::Either GenError (String, [Cursor])->Either GenError (String, [Cursor])->Either GenError (String, [Cursor])
x +++ y = case x of
    Right (s1, _) -> case y of
        Right (s2, c2) -> Right (s1 ++ s2, c2)
        Left err -> Left err
    Left err -> Left err

(<@>)::Cursor->String->Maybe String
c <@> name = case attribute (Name (pack name) Nothing Nothing) c of
    [] -> Nothing
    list -> Just (concat (map unpack list))

next::Cursor->Maybe Cursor
next c = case followingSibling c of
    (first:_) -> Just first
    [] -> Nothing

cShow::Cursor->[Cursor]->Expression->String
cShow c remainingChildren e = showCursor c ++ " - [" ++(intercalate ", " (map showCursor remainingChildren)) ++ "] - " ++ show e

seq2String::Grammar->Sequence->Cursor->[Cursor]->Either GenError (String, [Cursor])

seq2String g [] c remainingChildren = Right ("", remainingChildren)
seq2String g (e:rest) c remainingChildren =
    case exp2String g e c remainingChildren of
        ret@(Right (s, nextRemainingChildren)) ->
            case seq2String g rest c nextRemainingChildren of
                Right (s2, nextRemainingChildren2) -> Right (s ++ s2, nextRemainingChildren2)
                Left err -> Left err
        ret -> (trace "Sequence Fail") $ ret







exp2String::Grammar->Expression->Cursor->[Cursor]->Either GenError (String, [Cursor])


exp2String g (Or [e]) c remainingChildren = seq2String g e c remainingChildren
exp2String g (Or (e:rest)) c remainingChildren =
    case seq2String g e c remainingChildren of
        Right s -> Right s
        Left _ -> exp2String g (Or rest) c remainingChildren

exp2String g (SepBy e separator) c remainingChildren =
    case seq2String g (e ++ [List (separator ++ e)]) c remainingChildren of
        Right ret -> Right ret
        Left err -> Right ("", remainingChildren)

exp2String g (SepBy1 e separator) c remainingChildren =
    case seq2String g (e ++ [List (separator ++ e)]) c remainingChildren of
        Right ret -> Right ret
        Left err -> Right ("", remainingChildren)

exp2String g fullE@(SepBy2 e separator) c remainingChildren = (trace (cShow c remainingChildren fullE)) $
    case seq2String g (e ++ [List (separator ++ e)]) c remainingChildren of
        Right ret -> Right ret
        Left err -> Right ("", remainingChildren)

--exp2String g fullE@(List e) c [] = trace "d" $ Left $ GenError "List expected, but no more nodes"
exp2String g fullE@(List e) c remainingChildren = (trace (cShow c remainingChildren fullE)) $
    case seq2String g e c remainingChildren of
        Right (s1, nextRemainingChildren) ->
            case exp2String g (List e) c nextRemainingChildren of
                Right (s2, nextRemainingChildren2) -> trace "a" $ Right (s1 ++ s2, nextRemainingChildren2)
                Left err2 -> trace "b" $ Right (s1, nextRemainingChildren)
        Left err -> trace "c" $ Right ("", remainingChildren)

exp2String g fullE@(Link name) c [] = trace "1" $
    Left $ GenError ("Looking for element with tagname '" ++ name ++ "', but there are no more elements")
exp2String g fullE@(Link name) c remainingChildren | tagName (head remainingChildren) == name =
    (trace (cShow c remainingChildren fullE)) $ (trace ("Found " ++ name)) $
        case cursor2String g (head $ remainingChildren) of
            Right s -> Right (s, tail remainingChildren)
            Left err -> Left $ GenError "abcd"
{--            Left err -> (trace ("Looking up '" ++ name ++ "' in grammar assignments")) $ case lookup name (assignments g) of
                Just exp -> trace "found" $ exp2String g exp c remainingChildren
                Nothing -> Left err--}
exp2String g fullE@(Link name) c remainingChildren = (trace ("Looking up link '" ++ name ++ "' 2")) $
    case lookup name (assignmentMap g) of
        Just exp -> trace "found" $ seq2String g exp c remainingChildren
        Nothing -> (trace "Not found") $ Left $ GenError ("Expecting element with tagname '" ++ name ++ "', found " ++ showCursor c)

exp2String g fullE@(Blank) c remainingChildren = Right ("", remainingChildren)

exp2String g fullE@(MultiElementWrapper name exp) c [] = Left  $ GenError "qqqq" --(trace "1") $exp2String g exp c []
exp2String g fullE@(MultiElementWrapper name exp) c remainingChildren | tagName (head remainingChildren) == name =
    (trace ("Looking up matched operator'" ++ name ++ "'")) $
    case seq2String g exp (head remainingChildren) (child $ head remainingChildren) of
        Right (s, []) -> Right (s, tail remainingChildren)
        Right (s, _) -> Left $ GenError ("There was some extra stuff in an element: In " ++ showCursor c ++
                                        " is\n    " ++
                                        (intercalate " ---- " (map showCursor remainingChildren)))
        Left err -> Left err

exp2String g fullE@(MultiElementWrapper name exp) c remainingChildren =
    (trace ("descending in multielemementwrapper '" ++ name ++ "'")) $
    seq2String g exp c remainingChildren

exp2String g fullE@(Reparse second first) c remainingChildren =
    seq2String g second c remainingChildren

exp2String g fullE@(Attribute name _) c remainingChildren = (trace (cShow c remainingChildren fullE)) $
    case c <@> name of
        Just value -> Right (value, remainingChildren)
        Nothing -> Left $ GenError ("Missing attribute '" ++ name ++ "' in xml snippet \n    " ++ show c)

exp2String g fullE@(StringOf _) c remainingChildren = (trace (cShow c remainingChildren fullE)) $
    Right (concat (map unpack (child c >>= content)), [])

exp2String g fullE@(TextMatch text) c remainingChildren = (trace (cShow c remainingChildren fullE)) $ Right (text, remainingChildren)
exp2String g fullE@(WhiteSpace defaultValue) c remainingChildren = (trace (cShow c remainingChildren fullE)) $ Right (defaultValue, remainingChildren)
exp2String g (Tab tabString e) c remainingChildren =
    fmap (\(s, remainingChildren) -> ("\t" ++ tabString ++ "\t" ++ s ++ "\b", remainingChildren)) (seq2String g e c remainingChildren)

exp2String g e c remainingChildren = (trace (cShow c remainingChildren e)) $ error ("Error: tag = " ++ tagName c ++ ", expression = " ++ show e)









