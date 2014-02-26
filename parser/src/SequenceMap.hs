{-# OPTIONS_GHC -Wall #-}

module SequenceMap (
    SequenceMap,
    sequenceMap,
    formatSequenceMap,
    fillInWSSeqs
) where

import Control.Arrow
import Control.Lens
import Data.Functor
import Data.List hiding (union)
import Data.Map as M hiding (filter)
import Data.Tuple

import CharSet
import EnhancedString
import Format
import Grammar
import GrammarTools
import SequenceTools

--import JDebug

type SequenceMap = Map Name Sequence

formatSequenceMap::SequenceMap->String
formatSequenceMap sMap = concat ((++ "\n\n") <$> formatSubstitution <$> (toList sMap))

formatSubstitution::(String, Sequence)->String
formatSubstitution (ruleName, sq) = ruleName ++ " => " ++ format sq

sequenceMap::Grammar->SequenceMap
sequenceMap g =
    union
        (fromList (fmap (classSequence g) <$> (M.toList $ g^.classes)))
        (fmap orify (fromListWith (++) (((^.name)&&&((:[]) . (^.rawSequence))) <$> allRules)))
            where
                allRules = elems (g^.classes) >>= (^.rules)

classSequence::Grammar->Class->Sequence
classSequence g cl =
    (if classIsBlocking then [Out [StartBlock]] else [])
        ++ prefix ++ (if length suffix == 0 then [] else [List 0 suffix])
    ++ (if classIsBlocking then [Out [EndBlock]] else [])
    where
        classIsBlocking = isBlockClass g cl
        prefix = orify (nonSelfRule ++ selfRule)
        selfRule::[Sequence]
        selfRule = (^.rawSequence) <$> filter ((== cl^.className ) . (^.name)) (cl^.rules)
        nonSelfRule::[Sequence]
        nonSelfRule = (:[]) <$> Link <$>
                      filter (/= cl^.className) (nub $ ((^.name) <$> cl^.rules) ++ (cl^.parentNames))
        suffix = orify (cl^.suffixSeqs)

getIt::Ord k=>Map k a->k->a->a
getIt m k v = case M.lookup k m of
  Nothing -> v
  Just x -> x

--I will assume that the sequences inputted for whitespace are simple....  They should only contain Character, TextMatch, and Lists (which at this point could include EQuote)
--Note that I don't have any checks (yet) to enforce this, so the user could break us by adding something more complicated.
--If after time I am convinced that this restriction is OK, I will probably add those checks.
fillInWSSeqs::Grammar->SequenceMap->SequenceMap
fillInWSSeqs g sMap = mapWithKey (\rulename -> (>>= eModify (addWSSeq (getIt wsMap rulename [])))) sMap
  where
    addWSSeq::[Sequence]->Expression->Sequence
    addWSSeq wsSeqs (WhiteSpace _ dfltWS) = [WhiteSpace ((>>= eModify normalizeWSExp) <$> wsSeqs) dfltWS]
    addWSSeq _ x = [x]

    wsSeq = [TextMatch "/*" Nothing, List 0 [Character (CharSet False [Any]) Nothing], TextMatch "*/" Nothing]

    wsMap::Map Name [Sequence]
    wsMap = fromList $ class2wsTuples =<< (elems $ g^.classes)

    class2wsTuples::Class->[(String, [Sequence])]
    class2wsTuples cl = (\n -> (n, cl^.whiteSpaceSequences)) <$> (^.name) <$> cl^.rules

    normalizeWSExp::Expression->Sequence
    normalizeWSExp (EQuote count sq) = [List count sq] --Under the assumptions of simplicity, EQuote=SepBy=List
    normalizeWSExp x = [x]

