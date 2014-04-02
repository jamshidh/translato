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
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M hiding (filter, null, map, (\\))
import Text.Regex.Posix

import EnhancedString
import Format
import Grammar
import GrammarParser
import OperatorNames
import SequenceTools

--import JDebug

isA::Grammar->ClassName->ClassName->Bool
isA _ className1 className2 | className1 == className2 = True
isA g _ tagName | not (tagName `elem` (M.keys $ g^.classes)) = False -- If name2 doesn't refer to a class, then they must match exactly
isA g className1 className2
    | className1 `elem` className2Class g className2 ^.. rules.traversed.name = True
--isA g className1 className2 | className1 `elem` (symbol2Name <$> symbol <$> (operators $ className2Class g className2)) = True
isA g className1 className2 = or (isA g className1 <$> className2ParentNames g className2)

className2ParentNames::Grammar->ClassName->[ClassName]
className2ParentNames g tagName =
    case M.lookup tagName (g^.classes) of
        Just cl -> cl^.parentNames
        Nothing ->
            case Prelude.lookup tagName ruleName2ClassName of
                Just clsName -> [clsName]
                Nothing -> error ("'" ++ TL.unpack tagName ++ "' is not a className or ruleName.")
    where
        ruleName2ClassName::[(Name, ClassName)]
        ruleName2ClassName = --TODO Shrink this if you can
            (\cl -> ((^.name)&&&const (cl^.className)) <$> cl^.rules) =<< M.elems (g^.classes)

className2Class::Grammar->ClassName->Class
className2Class g clsName =
    case M.lookup clsName (g^.classes) of
        Just cl -> cl
        Nothing -> error ("ClassName '" ++ TL.unpack clsName ++ "' doesn't exist.")

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

isLRecursive::ClassName->Rule->Bool
isLRecursive clName r =
    case r^.rawSequence of
        (Link Nothing linkName:_) -> linkName == r^.name || linkName == clName
        _ -> False

---------------------

rewriteOperators::Grammar->Class->Class
rewriteOperators g cl = 
  (operators .~ [])
  $ (suffixSeqs %~ (++ (operator2SuffixSeq g cl <$> cl^.operators))) cl

op2Infix::ClassName->Operator->EChar
op2Infix className oprtr = InfixTag
    InfixOp{
        opPriority=oprtr^.priority,
        opName=className `TL.append` symbol2Name (oprtr^.symbol),
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
    oprtr^.symbol ++ [Out [op2Infix (theClass^.className) oprtr], Or ((:[]) . Link Nothing <$> nonRecursiveRuleNames theClass)]
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
                ((cls^.className) `TL.append` (symbol2Name $ o^.symbol))
                ([Link Nothing (cls^.className)] ++ o^.symbol ++ [Link Nothing (cls^.className)])


---------------------

{-
addEOFToGrammar::Grammar->Grammar
addEOFToGrammar g =
    classes.mapped.(filtered ((g^.main ==) . (^.className))).rules.mapped.rawSequence %~ (++ [EOF]) $ g
-}

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


        addTagToSequence::TL.Text->Sequence->Sequence
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
seq2Separator g [Link _ linkName] = case M.lookup linkName (g^.classes) of
    Nothing -> [] --If linkName isn't a class, then it is a ruleName (or it could also be
                    --a type, but it will be caught elsewhere)
    Just cl -> cl^.separator
seq2Separator _ [Character _ _] = []
seq2Separator _ [TextMatch _ _] = []
seq2Separator _ sq | length sq > 1 = []
seq2Separator _ sq = error ("Missing case in seq2Separator: " ++ format sq)

-------------------------------

fillInWSSeqs::Grammar->Grammar
fillInWSSeqs g = (classes.mapped %~ fillInWSSeqsInClass g) g

fillInWSSeqsInClass::Grammar->Class->Class
fillInWSSeqsInClass g c =
    ((separator %~ fillWSSeqsInSeq wsMap (c ^. className))
        . (left %~ fillWSSeqsInSeq wsMap (c ^. className))
        . (right %~ fillWSSeqsInSeq wsMap (c ^. className))
        . (operators.mapped.symbol %~ fillWSSeqsInSeq wsMap (c ^. className))
        . (rules.mapped.rawSequence %~ fillWSSeqsInSeq wsMap (c ^. className))
        . (suffixSeqs.mapped  %~ fillWSSeqsInSeq wsMap (c ^. className))) c
    where
      wsMap::M.Map Name [Sequence]
      wsMap = (map (eModify normalizeWSExp =<<)) <$> (M.fromList $ class2wsTuples =<< (M.elems $ g^.classes))

      class2wsTuples::Class->[(ClassName, [Sequence])]
      class2wsTuples cl = ((\n -> (n, cl^.whiteSpaceSequences)) <$> (^.name) <$> cl^.rules)
                                 ++ [(cl^.className, cl^.whiteSpaceSequences)]

      normalizeWSExp::Expression->Sequence 
      normalizeWSExp (EQuote count sq) = [List count sq] --Under the assumptions of simplicity, EQuote=SepBy=List
      normalizeWSExp x = [x]


--I will assume that the sequences inputted for whitespace are simple....  They should only contain Character, TextMatch, and Lists (which at this point could include EQuote)
--Note that I don't have any checks (yet) to enforce this, so the user could break us by adding something more complicated.
--If after time I am convinced that this restriction is OK, I will probably add those checks.
fillWSSeqsInSeq::M.Map ClassName [Sequence]->ClassName->Sequence->Sequence
fillWSSeqsInSeq wsSeqMap className [] = []
-- Any whitespace surrounding a "Link" inherits the wsSeqs from that link, not the surrounding tag.
-- Note, this includes "Link"s inside EQuotes.
fillWSSeqsInSeq wsSeqMap className (WhiteSpace _ dfltWS:e@(Link _ linkName):rest) = WhiteSpace (getWSSeq linkName wsSeqMap) dfltWS:fillWSSeqsInSeq wsSeqMap className (e:rest)
fillWSSeqsInSeq wsSeqMap className (WhiteSpace _ dfltWS:e@(EQuote _ [Link _ linkName]):rest) = WhiteSpace (getWSSeq linkName wsSeqMap) dfltWS:fillWSSeqsInSeq wsSeqMap className (e:rest)
fillWSSeqsInSeq wsSeqMap className (e@(Link _ linkName):WhiteSpace _ dfltWS:rest) = e:WhiteSpace (getWSSeq linkName wsSeqMap) dfltWS:fillWSSeqsInSeq wsSeqMap className rest
fillWSSeqsInSeq wsSeqMap className (e@(EQuote _ [Link _ linkName]):WhiteSpace _ dfltWS:rest) = e:WhiteSpace (getWSSeq linkName wsSeqMap) dfltWS:fillWSSeqsInSeq wsSeqMap className rest
-- All other whitespaces get their wsSeqs from the surrounding tag.
fillWSSeqsInSeq wsSeqMap className (WhiteSpace _ dfltWS:rest) = WhiteSpace (getWSSeq className wsSeqMap) dfltWS:fillWSSeqsInSeq wsSeqMap className rest

fillWSSeqsInSeq wsSeqMap className (Or seqs:rest) = Or (fillWSSeqsInSeq wsSeqMap className <$> seqs):fillWSSeqsInSeq wsSeqMap className rest
fillWSSeqsInSeq wsSeqMap className (List minCount sq:rest) = List minCount (fillWSSeqsInSeq wsSeqMap className sq):fillWSSeqsInSeq wsSeqMap className rest
fillWSSeqsInSeq wsSeqMap className (SepBy minCount sq sep:rest) = SepBy minCount (fillWSSeqsInSeq wsSeqMap className sq) (fillWSSeqsInSeq wsSeqMap className sep):fillWSSeqsInSeq wsSeqMap className rest
fillWSSeqsInSeq wsSeqMap className (EQuote minCount sq:rest) = EQuote minCount (fillWSSeqsInSeq wsSeqMap className sq):fillWSSeqsInSeq wsSeqMap className rest
fillWSSeqsInSeq wsSeqMap className (Option sq:rest) = Option (fillWSSeqsInSeq wsSeqMap className sq):fillWSSeqsInSeq wsSeqMap className rest
fillWSSeqsInSeq wsSeqMap className (e:rest) = e:fillWSSeqsInSeq wsSeqMap className rest

getWSSeq::ClassName->M.Map ClassName [Sequence]->[Sequence]
getWSSeq name theMap =
      case M.lookup name theMap of
        Nothing -> error ("Error in fillWSSeqsInSeq: Missing name in wsSeqMap: " ++ TL.unpack name)
        Just x -> x




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
seq2Left g [Link _ linkName] = case M.lookup linkName (g^.classes) of
    Nothing -> [] --If linkName isn't a class, then it is a ruleName (or it could also be
                    --a type, but it will be caught elsewhere)
    Just cl -> cl^.left
seq2Left _ [Character _ _] = []
seq2Left _ [TextMatch _ _] = []
seq2Left _ sq | length sq > 1 = []
seq2Left _ sq = error ("Missing case in seq2Left: " ++ show sq)

seq2Right::Grammar->Sequence->Sequence
seq2Right g [Link _ linkName] = case M.lookup linkName (g^.classes) of
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
        addTabsToSeq (WhiteSpace wsSeq (WSString defltWS):expr@(Link _ _):rest) =  rebuildIt wsSeq defltWS expr rest
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
        --I am moving the addition of the EOF to the funtion "parseTree", because as of right now, and EOF is needed for all sequences (chooseOne seems to explode otherwise), and since a reparse is using non root production rules as the basis, we need to add them elsewhere (not to mention that now we can use the root rule embedded elsewhere).
--        $ addEOFToGrammar
        classes.mapped.rules.mapped %~ addTagToRule
        $ modifyGrammar rewriteOperators
        -- $ modifyGrammar addInheritedSuffixes 
        $ classes.mapped %~ rewriteLeftRecursion 
        -- $ modifyGrammar addInheritedOperators 
        $ adjustPrioritiesByClassHiarchy --This has to happen before rewriteOperators and rewriteLeftRecursion or else Infix operators won't have the correct priorities
        $ removeOption
        $ removeSepBy
        $ removeEQuote
        $ stripWhitespaceFromGrammar
        $ fillInWSSeqs g)
    where
      modifyGrammar::(Grammar->Class->Class)->Grammar->Grammar
      modifyGrammar f g = (classes.mapped %~ f g) g

loadGrammarAndSimplifyForGenerate::SpecName->IO Grammar
loadGrammarAndSimplifyForGenerate specName = do
    g <- loadUnsimplifiedGrammar specName
    return (
        adjustPrioritiesByClassHiarchy
--        $ addEOFToGrammar
        $ rewriteOperatorsAsLeftRecursion
        -- $ classes.mapped %~ addInheritedOperators g
        $ removeOption
        $ addTabs
        $ removeEQuote
        $ stripWhitespaceFromGrammar g
        )
