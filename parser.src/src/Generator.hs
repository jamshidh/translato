{-# Language TemplateHaskell #-}
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
    GenError (GenError),
    generatorMain
) where

import Prelude hiding (lookup)
import Data.Functor
import Data.Text hiding (map, concat, foldl1, foldl, head, intercalate, tail)
import Data.Text.Lazy.IO as TL hiding (putStrLn, interact)
import Data.Map hiding (map, foldl)
import Data.Maybe
import Data.List hiding (lookup)
import Text.XML
import Text.XML.Cursor
import Control.Arrow
import System.Console.GetOpt

import Debug.Trace

--import OperatorNames
import ArgOpts
import Colors
import Grammar hiding (tagName, Name)
import GrammarParser
import GrammarTools
import EnhancedString
import qualified LString as LS
import SequenceMap

------------------------------

name2String::Name->String
name2String name = unpack $ nameLocalName name

tagName::Cursor->String
--tagName (Cursor (NodeElement element)) = nameLocalName $ elementName element
tagName c = case node c of
    NodeElement element -> name2String $ elementName element
    x -> red ("<<<Not a tag: " ++ showCursor c ++ ">>>>") --error ("Missing case in tagName: " ++ show x)

showCursor::Cursor->String
showCursor c = case node c of
    NodeElement element ->
        "<" ++ tagName c ++ " "
            ++ (intercalate " "
                    (map (\(name, text) -> name2String name ++ "=\"" ++ unpack text ++ "\"")
                    ((toList . elementAttributes) element))) ++ ">"
    NodeContent text -> "Text:'" ++ unpack text

-------------------------------

data GenError = GenError String deriving (Show)

expand::([a], b)->[(a, b)]
expand (x:rest, y)=(x, y):(expand (rest, y))



generate::Grammar->Cursor->Either GenError String
generate g c = fmap enhancedString2String (cursor2String (sequenceMap g) (head (child c >>= anyElement)))

cursor2String::SequenceMap->Cursor->Either GenError EString
cursor2String sMap c = case lookup (tagName c) sMap of
    Just seq ->
        case seq2EString sMap seq c (child c) of
            Right (s, _) -> Right s
            Left e -> Left e
    Nothing -> error ("Link '" ++ tagName c ++ "' doesn't exist in the grammar")

cursorAndSequences2String::SequenceMap->[Sequence]->Cursor->[Cursor]->Either GenError (EString, Maybe Cursor)
cursorAndSequences2String g (e:rest) c remainingChildren =
    case seq2EString g e c remainingChildren of
        (Right (s, [])) -> Right (s, next c)
        (Right (s, _)) ->
            if (rest == []) then
                    Left $ GenError ("There was some extra stuff in an element: In " ++ showCursor c ++ " is\n    " ++
                                        (intercalate " ---- " (map showCursor remainingChildren)))
                else cursorAndSequences2String g rest c remainingChildren
        Left e -> if (rest == []) then Left e
                else cursorAndSequences2String g rest c remainingChildren
cursorAndSequences2String g [] c remainingChildren = error "Shouldn't be here"

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

seq2EString::SequenceMap->Sequence->Cursor->[Cursor]->Either GenError (EString, [Cursor])
seq2EString sMap [] c remainingChildren = Right (e "", remainingChildren)
seq2EString sMap (e:rest) c remainingChildren =
    case exp2String sMap e c remainingChildren of
        (Right (s, nextRemainingChildren)) ->
            case seq2EString sMap rest c nextRemainingChildren of
                Right (s2, nextRemainingChildren2) -> Right (s ++ s2, nextRemainingChildren2)
                Left err -> Left err
        ret -> ret

exp2String::SequenceMap->Expression->Cursor->[Cursor]->Either GenError (EString, [Cursor])
exp2String sMap (List 1 exp) c remainingChildren =
    case seq2EString sMap (exp ++ [List 0 exp]) c remainingChildren of
        Right ret -> Right ret
        Left err -> Right (e "", remainingChildren)
exp2String sMap (List min exp) c remainingChildren =
    case seq2EString sMap exp c remainingChildren of
        Right (s1, nextRemainingChildren) ->
            case exp2String sMap (List min exp) c nextRemainingChildren of
                Right (s2, nextRemainingChildren2) -> Right (s1 ++ s2, nextRemainingChildren2)
                Left err2 -> Right (s1, nextRemainingChildren)
        Left err -> Right (e "", remainingChildren)

exp2String sMap (Link name) c [] =
    Left $ GenError ("Looking for element with tagname '" ++ name ++ "', but there are no more elements")
exp2String sMap (Link name) c remainingChildren | tagName (head remainingChildren) == name =
        case cursor2String sMap (head $ remainingChildren) of
            Right s -> Right (s, tail remainingChildren)
            Left err -> Left $ GenError "abcd"
{--            Left err -> (trace ("Looking up '" ++ name ++ "' in grammar assignments")) $ case lookup name (assignments g) of
                Just exp -> trace "found" $ exp2String g exp c remainingChildren
                Nothing -> Left err--}
exp2String sMap (Link name) c remainingChildren =
        Left $ GenError ("Expecting element with tagname '" ++ name ++ "', found " ++ showCursor c)

{--exp2String sMap fullE@(Reparse second first) c remainingChildren =
    seq2EString sMap second c remainingChildren--}

exp2String sMap (Out [VStart name _]) c remainingChildren =
    case c <@> name of
        Just value -> Right (e value, remainingChildren)
        Nothing -> Left $ GenError ("Missing attribute '" ++ name ++ "' in xml snippet \n    " ++ show c)

exp2String sMap (TextMatch text _) c remainingChildren = Right (e text, remainingChildren)
exp2String sMap (WhiteSpace defaultValue) c remainingChildren = Right (e defaultValue, remainingChildren)
exp2String sMap (Out [TabRight tabString]) c remainingChildren = Right ([TabRight tabString], remainingChildren)
exp2String sMap (Out [TabLeft]) c remainingChildren = Right ([TabLeft], remainingChildren)

exp2String sMap (Out [EStart _ _]) c remainingChildren = Right ([], remainingChildren)

exp2String sMap exp c remainingChildren =
    --(trace (cShow c remainingChildren e)) $
    case node c of
        NodeContent _ -> Right ([], remainingChildren)
        _ -> error ("Error: tag = " ++ tagName c ++ ", expression = " ++ formatExpression exp)

----------------

data Options = Options { specFileName::String }
defaultOptions = Options { specFileName = "file.spec" }

deflt::Options
deflt = Options{specFileName="qqqq.spec"}

generatorMain::[String]->IO ()
generatorMain args = do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar <- loadGrammar $ specFileName options
    contents<-TL.getContents
    let doc=case parseText def contents of
                Left err -> error ("Error:" ++ show err)
                Right x -> x
    case generate grammar (fromDocument doc) of
        Right s -> putStrLn s
        Left err -> error (show err)









