-----------------------------------------------------------------------------
--
-- Module      :  GrammarNormalizer
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

module GrammarNormalizer (
    normalizeGrammar,
    fullSequence
) where

normalizeGrammar::Grammar->Grammar

normalizeClass::Class->Class

rule2PostSequence::Rule->[Sequence]
rule2PostSequence rule = if isLRecursive rule then
        [((G.InfixTag 9 (tagName rule)):((tail (rawSequence rule))))]
            else []

parents::Grammar->Class->[Class]
parents g cl =
    filter (\cl2 -> elem (className cl2) (parentNames cl)) (snd <$> (toList (classes g)))

class2AllOpSymbols::Grammar->Class->[OperatorSymbol]
class2AllOpSymbols g c = operators c
                ++ concat (map (class2AllOpSymbols g) (map (fromJust . (name2Class g)) (parentNames c)))

