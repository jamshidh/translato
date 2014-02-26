{-# OPTIONS_GHC -Wall #-}

module GrammarTools (
    isA,
    orify,
    prepend,
    isBlockClass,
    removeOption,
    removeEQuote,
    removeSepBy,
    loadUnsimplifiedGrammar,
    loadGrammarAndSimplifyForParse,
    loadGrammarAndSimplifyForGenerate
) where

import Control.Arrow hiding (left, right, (+++))
import Control.Lens
import Data.Functor
import qualified Data.Map as M hiding (filter, null, map, (\\))
import Text.Regex.Posix

import EnhancedString
import Format
import Grammar
import GrammarParser
import OperatorNames

--import JDebug

isA::Grammar->String->String->Bool
isA _ className1 className2 | className1 == className2 = True
isA g _ tagName | not (tagName `elem` (M.keys $ g^.classes)) = False -- If name2 doesn't refer to a class, then they must match exactly
isA g className1 className2
    | className1 `elem` className2Class g className2 ^.. rules.traversed.name = True
--isA g className1 className2 | className1 `elem` (symbol2Name <$> symbol <$> (operators $ className2Class g className2)) = True
isA g className1 className2 = or (isA g className1 <$> className2ParentNames g className2)

className2ParentNames::Grammar->String->[String]
className2ParentNames g tagName =
    case M.lookup tagName (g^.classes) of
        Just cl -> cl^.parentNames
        Nothing ->
            case Prelude.lookup tagName ruleName2ClassName of
                Just clsName -> [clsName]
                Nothing -> error ("'" ++ tagName ++ "' is not a className or ruleName.")
    where
        ruleName2ClassName::[(String, String)]
        ruleName2ClassName = --TODO Shrink this if you can
            (\cl -> ((^.name)&&&const (cl^.className)) <$> cl^.rules) =<< M.elems (g^.classes)

className2Class::Grammar->String->Class
className2Class g clsName =
    case M.lookup clsName (g^.classes) of
        Just cl -> cl
        Nothing -> error ("ClassName '" ++ clsName ++ "' doesn't exist.")

stripWhitespaceFromGrammar::Grammar->Grammar
stripWhitespaceFromGrammar g =
    classes.mapped.rules.mapped.(filtered ((g^.main /=) . (^.name))).rawSequence %~ strip $ g

strip::Sequence->Sequence
strip (WhiteSpace _ _:rest) = removeLastWhitespace rest
strip x = removeLastWhitespace x

removeLastWhitespace::Sequence->Sequence
removeLastWhitespace (expr:[WhiteSpace _ _]) = [expr]
removeLastWhitespace (expr:rest) = expr:removeLastWhitespace rest
removeLastWhitespace [] = []

---------------------

{-addInheritedRulesInGrammar::Grammar->Grammar
addInheritedRulesInGrammar g = (classes.mapped %~ addInheritedRules g) g

addInheritedRules::Grammar->Class->Class
addInheritedRules g cl = 
  (rules %~ (++ (eld >>= (^.rules)))) $
  (operators %~ (++ (eld >>= (^.operators)))) $
  (parentNames .~ []) cl
  where
    eld = elders g cl-}

{-
addInheritedOperators::Grammar->Class->Class
addInheritedOperators g cl = operators %~ (++ (elders g cl >>= (^.operators))) $ cl

addInheritedSuffixes::Grammar->Class->Class
addInheritedSuffixes g cl = suffixSeqs %~ (++ (elders g cl >>= (^.suffixSeqs))) $ cl
-}

---------------------

rewriteLeftRecursion::Class->Class
rewriteLeftRecursion cl =
    (rules %~ (filter (not . (isLRecursive $ cl^.className))))
        $ (suffixSeqs .~ class2SuffixSeq cl) cl
    where
        class2SuffixSeq::Class->[Sequence]
        class2SuffixSeq cls =
                recursiveRule2SuffixSeq <$> (filter (isLRecursive $ cls^.className) (cls^.rules))

recursiveRule2SuffixSeq::Rule->Sequence --TODO Unitary operators
recursiveRule2SuffixSeq r = (Out [InfixTag infixOp]:) $ tail $ (r^.rawSequence) ++ [Out [EndCap (r^.name)]]
    where
        infixOp =
            InfixOp {
                opPriority = r^.rulePriority,
                opName = r^.name,
                opAssociativity = UseEndCap
            }

isBlockClass::Grammar->Class->Bool
isBlockClass g cl =
    or (isLRecursive (cl^.className) <$> cl^.rules)
        || (not $ null $ cl^.operators)
        || (not $ null $ cl^.suffixSeqs)
        || or (isBlockClass g <$> parents g cl)

isLRecursive::String->Rule->Bool
isLRecursive clName r =
    case r^.rawSequence of
        (Link linkName:_) -> linkName == r^.name || linkName == clName
        _ -> False

---------------------

rewriteOperators::Grammar->Class->Class
rewriteOperators g cl = 
  (operators .~ [])
  $ (suffixSeqs %~ (++ (operator2SuffixSeq g cl <$> cl^.operators))) cl

op2Infix::Operator->EChar
op2Infix oprtr = InfixTag
    InfixOp{
        opPriority=oprtr^.priority,
        opName=symbol2Name (oprtr^.symbol),
        opAssociativity=oprtr^.associativity
    }


--It is very important that you filter any recursive rule before adding the operator suffix....
--In other words, for operator "+", the suffix should be 
--        ("+" {non recursive part of expression})*
--not
--        ("+" {expression})*
--else you will get an insidious "multiple matching" bug in the lookahead module....
--Tracking this down can be very difficult (....as I have learned).
operator2SuffixSeq::Grammar->Class->Operator->Sequence
operator2SuffixSeq g theClass oprtr =
    oprtr^.symbol ++ [Out [op2Infix oprtr], Or ((:[]) . Link <$> nonRecursiveRuleNames theClass)]
    where nonRecursiveRuleNames cl' = ((^.name) <$> filter (not . isLRecursive (cl'^.className)) (cl'^.rules))
                                        ++ ((^.className) <$> parents g cl')

---------------------

rewriteOperatorsAsLeftRecursion::Grammar->Grammar
rewriteOperatorsAsLeftRecursion g =
    classes %~ (rewriteOperatorsAsLeftRecursionInClass <$>) $ g

rewriteOperatorsAsLeftRecursionInClass::Class->Class
rewriteOperatorsAsLeftRecursionInClass cls =
    rules %~ (++ (operator2LeftRecursiveRule <$> cls^.operators))
        $ (operators .~ []) cls
    where
        operator2LeftRecursiveRule::Operator->Rule
        operator2LeftRecursiveRule o =
            Rule
                (o^.priority)
                (symbol2Name $ o^.symbol)
                ([Link (cls^.className)] ++ o^.symbol ++ [Link (cls^.className)])


---------------------

addEOFToGrammar::Grammar->Grammar
addEOFToGrammar g =
    classes.mapped.(filtered ((g^.main ==) . (^.className))).rules.mapped.rawSequence %~ (++ [EOF]) $ g

---------------------

prepend::Expression->Sequence->Sequence
prepend (Out eSequence1) (Out eSequence2:rest) = Out(eSequence1 ++ eSequence2):rest
prepend expr@(Out _) (Or seqs:rest) = Or ((expr `prepend`) <$> seqs):rest
prepend expr seqn = expr:seqn

orify::[Sequence]->Sequence
orify [sq] = sq
orify [] = []
orify seqs = [Or (seqs >>= removeOr)]
    where
        removeOr (Or sqs:rest) = (++ rest) <$> sqs
        removeOr sq = [sq]

----

addToClassPriorities::Int->Class->Class
addToClassPriorities p =
        (rules.mapped.rulePriority %~ (p+))
            . (operators.mapped.priority %~ (p+))

maxPriority::Class->Int
maxPriority cl = maximum (cl^..rules.traversed.rulePriority ++ cl^..operators.traversed.priority)

classToPriorityOffset::Grammar->Class->Int
classToPriorityOffset g cl = 1 + (sum $ (1+) <$> maxPriority <$> parents g cl)

adjustClass::Grammar->Class->Class
adjustClass g cl = addToClassPriorities (classToPriorityOffset g cl) cl

adjustPrioritiesByClassHiarchy::Grammar->Grammar
adjustPrioritiesByClassHiarchy g = (classes.mapped %~ adjustClass g) g

---------------------------

addTagToRule::Rule->Rule
addTagToRule r = (rawSequence %~ addTagToSequence (r^.name)) r
    where
        seq2AttInfoMap [] = M.empty
        seq2AttInfoMap (Out [VStart tagName _]:rest) =
            M.insertWith (||) tagName False (seq2AttInfoMap rest)
        seq2AttInfoMap (Or seqs:rest) = M.unionWith (||) joinedAttInfoMap $ seq2AttInfoMap rest
            where
                joinedAttInfoMap = --optional only false if false in every option
                    fmap ((/= length seqs) . length . (filter not))
                        $ foldl (M.unionWith (++)) M.empty
                        $ fmap (:[]) <$> attInfoMaps
                attInfoMaps = seq2AttInfoMap <$> seqs
        seq2AttInfoMap (_:rest) = seq2AttInfoMap rest


        addTagToSequence::String->Sequence->Sequence
        addTagToSequence tagName sq =
            Out [EStart tagName (seq2AttInfoMap sq)] `prepend` sq ++ [Out [EEnd tagName]]

----------------------------

removeEQuote::Grammar->Grammar
removeEQuote g = (classes.mapped %~ removeEQuoteFromClass g) g

removeEQuoteFromClass::Grammar->Class->Class
removeEQuoteFromClass g =
    (separator .~ [])
        . (left .~ [])
        . (right .~ [])
        . (rules.mapped.rawSequence %~ (>>= replaceEQuote g))
        . (suffixSeqs.mapped %~ (>>= replaceEQuote g))

replaceEQuote::Grammar->Expression->Sequence
replaceEQuote g (EQuote minCount sq) =
    (seq2Left g sq' >>= replaceEQuote g)
    ++ [SepBy minCount sq' (seq2Separator g sq' >>= replaceEQuote g)]
    ++ (seq2Right g sq' >>= replaceEQuote g)
    where
        sq' = sq >>= replaceEQuote g
replaceEQuote g (List minCount sq) = [List minCount (sq >>= replaceEQuote g)]
replaceEQuote g (Or seqs) = [Or ((>>= replaceEQuote g) <$> seqs)]
replaceEQuote g (Option sq) = [Option $ sq >>= replaceEQuote g]
replaceEQuote _ x = [x]

seq2Separator::Grammar->Sequence->Sequence
seq2Separator g [Link linkName] = case M.lookup linkName (g^.classes) of
    Nothing -> [] --If linkName isn't a class, then it is a ruleName (or it could also be
                    --a type, but it will be caught elsewhere)
    Just cl -> cl^.separator
seq2Separator _ [Character _ _] = []
seq2Separator _ [TextMatch _ _] = []
seq2Separator _ sq | length sq > 1 = []
seq2Separator _ sq = error ("Missing case in seq2Separator: " ++ format sq)

-------------------------------

modifySeqsInGrammar::(Sequence->Sequence)->Grammar->Grammar
modifySeqsInGrammar f = classes.mapped %~ ((rules.mapped.rawSequence %~ f) . (suffixSeqs.mapped %~ f))




removeSepBy::Grammar->Grammar
removeSepBy = modifySeqsInGrammar (replaceSepBy =<<)

replaceSepBy::Expression->Sequence
replaceSepBy (SepBy minCount sq sep) = repeatWithSeparator minCount sq (replaceSepBy =<< sep)
replaceSepBy (List minCount sq) = [List minCount (replaceSepBy =<< sq)]
replaceSepBy (Or seqs) = [Or ((replaceSepBy =<<) <$> seqs)]
replaceSepBy (Option sq) = [Option $ sq >>= replaceSepBy]
replaceSepBy x = [x]

repeatWithSeparator::Int->Sequence->Sequence->Sequence
repeatWithSeparator 0 sq sep = [Or [fixedSq ++ [List 0 (sep ++ fixedSq)], [Priority Low]]]
  where
    fixedSq = replaceSepBy =<< sq
repeatWithSeparator minCount sq sep = (replaceSepBy =<< sq) ++ [List (minCount -1) (replaceSepBy =<< sep++sq)]

seq2Left::Grammar->Sequence->Sequence
seq2Left g [Link linkName] = case M.lookup linkName (g^.classes) of
    Nothing -> [] --If linkName isn't a class, then it is a ruleName (or it could also be
                    --a type, but it will be caught elsewhere)
    Just cl -> cl^.left
seq2Left _ [Character _ _] = []
seq2Left _ [TextMatch _ _] = []
seq2Left _ sq | length sq > 1 = []
seq2Left _ sq = error ("Missing case in seq2Left: " ++ show sq)

seq2Right::Grammar->Sequence->Sequence
seq2Right g [Link linkName] = case M.lookup linkName (g^.classes) of
    Nothing -> []
    Just cl -> cl^.right
seq2Right _ [Character _ _] = []
seq2Right _ [TextMatch _ _] = []
seq2Right _ sq | length sq > 1 = []
seq2Right _ sq = error ("Missing case in seq2Separator: " ++ format sq)

-------------------------------

removeOption::Grammar->Grammar
removeOption g = modifySeqsInGrammar (replaceOption <$>) g
    where
        replaceOption::Expression->Expression
        replaceOption (Option sq) = Or [sq, [Priority Low]]
        replaceOption (List minCount sq) = List minCount (replaceOption <$> sq)
        replaceOption (Or seqs) = Or ((replaceOption <$>) <$> seqs)
        replaceOption x = x

addTabs::Grammar->Grammar
addTabs g = modifySeqsInGrammar addTabsToSeq g
    where
        addTabsToSeq::Sequence->Sequence
        addTabsToSeq [] = []
        addTabsToSeq (WhiteSpace wsSeq (WSString defltWS):expr@(Link _):rest) =  rebuildIt wsSeq defltWS expr rest
        addTabsToSeq (WhiteSpace wsSeq (WSString defltWS):expr@(SepBy _ _ _):rest) =  rebuildIt wsSeq defltWS expr rest
        addTabsToSeq (WhiteSpace wsSeq (WSString defltWS):expr@(List _ _):rest) =  rebuildIt wsSeq defltWS expr rest
        addTabsToSeq (expr:rest) = expr:addTabsToSeq rest

        rebuildIt wsSeq defltWS expr rest =
            case defltWS =~ "^(.*\n+)([^\\s]+)$" of
                [[_, prefixWS, tabSpaces]] ->
                    Out [TabRight tabSpaces]:WhiteSpace wsSeq (WSString prefixWS):expr:Out [TabLeft]:addTabsToSeq rest
                _ -> WhiteSpace wsSeq (WSString defltWS):addTabsToSeq (expr:rest)

-----------------------

loadUnsimplifiedGrammar::SpecName->IO Grammar
loadUnsimplifiedGrammar = parseFullGrammar

--Don't add inherited operators!
--I should just remove (not just comment out) the code that does this.
--Operators already effectively inherit anyway (ie- look at js.spec, lvalue operators work in expressons)
--Adding the code here causes insidious "multiple matching" errors in the lookahead module.
loadGrammarAndSimplifyForParse::SpecName->IO Grammar
loadGrammarAndSimplifyForParse specName = do
    g <- loadUnsimplifiedGrammar specName
    return (
        adjustPrioritiesByClassHiarchy
        $ addEOFToGrammar
        $ classes.mapped.rules.mapped %~ addTagToRule
        $ modifyGrammar rewriteOperators
        -- $ modifyGrammar addInheritedSuffixes 
        $ classes.mapped %~ rewriteLeftRecursion 
        -- $ modifyGrammar addInheritedOperators 
        $ stripWhitespaceFromGrammar
        $ removeOption
        $ removeSepBy
        $ removeEQuote g)
    where
      modifyGrammar::(Grammar->Class->Class)->Grammar->Grammar
      modifyGrammar f g = (classes.mapped %~ f g) g

loadGrammarAndSimplifyForGenerate::SpecName->IO Grammar
loadGrammarAndSimplifyForGenerate specName = do
    g <- loadUnsimplifiedGrammar specName
    return (
        adjustPrioritiesByClassHiarchy
        $ addEOFToGrammar
        $ rewriteOperatorsAsLeftRecursion
        -- $ classes.mapped %~ addInheritedOperators g
        $ stripWhitespaceFromGrammar
        $ removeOption
        $ addTabs
        $ removeEQuote g)
