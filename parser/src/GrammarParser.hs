-- {-# OPTIONS_GHC -Wall #-}

module GrammarParser (
    parseFullGrammar,
    Grammar(Grammar),
    RuleName,
    Sequence
) where

import Control.Arrow hiding (left, right)
import Control.Lens
import Data.Char hiding (Space)
import Data.Functor
import Data.List
import Data.Map as M hiding (filter, map, foldl)
import Data.Maybe
import qualified Data.Set as S
import System.FilePath
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr hiding (Operator)
import qualified Data.Set as Set

import Paths_parser

import Colors hiding (reverse)
import CharSet
import EnhancedString
import Grammar

import JDebug

--import Debug.Trace


specNameToSpecFile::FilePath->SpecName->IO FilePath
specNameToSpecFile baseDir specName = do
  if elem '.' specName || elem '/' specName
    then
    return $ baseDir </> specName
    else do
    specDir <- getDataFileName "specs"
    return $ specDir </> specName ++ ".spec"

--parses grammar, then parses and merges in all the recursive subgrammars
parseFullGrammar::SpecName->IO Grammar
parseFullGrammar specName = do
  let baseDir = takeDirectory specName
  specFile <- specNameToSpecFile baseDir specName
  builtinsFilePath <- getDataFileName "builtins.spec"
  
  foldl1 mergeGrammar 
    <$> parseFullGrammar' baseDir S.empty [specFile, builtinsFilePath]
  
  where
    parseFullGrammar'::FilePath->S.Set String->[String]->IO [Grammar]
    parseFullGrammar' _ _ [] = return []
    parseFullGrammar' baseDir obtained (needed:remainingNeeded) | needed `S.member` obtained = 
      parseFullGrammar' baseDir obtained remainingNeeded
    parseFullGrammar' baseDir obtained (needed:remainingNeeded) = do
      (grammar, subGrammars) <- loadGrammarAndSubGrammarNames $ baseDir </> needed
      subGrammarFiles <- sequence $ specNameToSpecFile baseDir <$> subGrammars
      remaining <- 
        parseFullGrammar' 
            baseDir 
            (S.insert needed obtained) 
            (remainingNeeded ++ subGrammarFiles)
      return (grammar:remaining)

loadGrammarAndSubGrammarNames::FilePath->IO (Grammar, [String])
loadGrammarAndSubGrammarNames specFile = do
  maybeResult <-
    parse parseGrammarAndSubGrammarNames specFile <$> readFile specFile
  case maybeResult of
    Left err -> error ("Error parsing grammar: " ++ show err)
    Right x -> return x
    
---------- Convert Lists to Maps (ie- create the Grammar from the parsed data)

data ClassOrSubGrammar = AClass Class | SomeSubGrammars [String]

parseGrammarAndSubGrammarNames::Parser (Grammar, [String])
parseGrammarAndSubGrammarNames =
    do
        spaces
        classesAndSubGrammars<-endBy parseClassOrSubGrammar spaces
        spaces
        eof

        let classList = [c|AClass c <- classesAndSubGrammars]
        let mainClassName =
                case classList of
                    (cl:_) -> cl^.className
                    _ -> error "Grammar contains no classes"

        return 
          (
            Grammar
              mainClassName
              (M.fromListWithKey 
                  (const . const . error . ("The grammar has a repeat element: " ++)) 
                  (((^.className)&&&id) <$> classList)),
            concat [subGrammars|SomeSubGrammars subGrammars<-classesAndSubGrammars]
          )


parseClassOrSubGrammar = 
  try (AClass <$> parseFullClass) 
  <|> try (AClass <$> parseSimpleClass)
  <|> (SomeSubGrammars <$> parseSubGrammars)

parseSimpleClass =
    do
        RuleItem rule<-parseRule
        spaces
        return (Class [rule] [] [] [WhiteSpace (WSString " ")] (rule^.name) [] [] [])

data ClassItem =
    RuleItem Rule
    | Comment
    | OperatorsItem [Operator]
    | SeparatorItem Sequence
    | LeftItem Sequence
    | RightItem Sequence

parseSubGrammars::Parser [String]
parseSubGrammars =
    do
        string "====<subgrammars>" >> many (char '=')
        spaces
        grammarNames <- endBy parseSubGrammarFilePath spaces
        string "====</subgrammars>" >> many (char '=')
        return grammarNames

parseSubGrammarFilePath::Parser String
parseSubGrammarFilePath = do
  string "<{"
  grammarName <- ident
  string "}>"
  return grammarName
  
parseFullClass::Parser Class
parseFullClass =
    do
        string "====["
        name<-ident
        parents <- option [] (char ':' >> sepBy ident (char ','))
        string "]"
        many (char '=')
        spaces
        items<-endBy (
            parseSeparator
            <|> parseLeft
            <|> parseRight
            <|> try parseOperators
            <|> parseRule
            <|> parseComment) spaces
        spaces
        string "====[/"
        name<-ident
        char ']'
        many (char '=')
        return (classWithPriorities items name parents)
        where
            classWithPriorities items name parents =
                (Class
                        [rule|RuleItem rule<-itemsWithPriority]
                        []
                        (concat [operators|OperatorsItem operators<-itemsWithPriority])
                        (getUnique
                                "separator"
                                [WhiteSpace (WSString " ")]
                                [separator|SeparatorItem separator<-itemsWithPriority])
                        name
                        (getUnique "left" [] [left|LeftItem left<-itemsWithPriority])
                        (getUnique "right" [] [right|RightItem right<-itemsWithPriority])
                        parents)
                where
                    itemsWithPriority = addPriorityToItems 0 items
                    getUnique::String->Sequence->[Sequence]->Sequence
                    getUnique name deflt items=case items of
                        [] -> deflt
                        [x] -> x
                        _ -> error ("Only one " ++ name ++ " allowed for a class")

            addPriorityToItems::Int->[ClassItem]->[ClassItem]
            addPriorityToItems p (RuleItem rule:rest) = RuleItem ((rulePriority `set` p) rule):addPriorityToItems (p+1) rest
            addPriorityToItems p (Comment:rest) = Comment:addPriorityToItems p rest
            addPriorityToItems p (OperatorsItem ops:rest) =
                        OperatorsItem ((priority %~ (p+)) <$> ops):addPriorityToItems (p+(length ops)) rest
            addPriorityToItems p (SeparatorItem separator:rest) = SeparatorItem separator:addPriorityToItems p rest
            addPriorityToItems p (LeftItem seq:rest) = LeftItem seq:addPriorityToItems p rest
            addPriorityToItems p (RightItem seq:rest) = RightItem seq:addPriorityToItems p rest
            addPriorityToItems _ _ = []

parseComment =
    do
        char '#'
        many (noneOf "\n")
        char '\n'
        return Comment

parseRule =
    do
        name<-ident
        spaces
        string "=>"
        sequence<-parseSequence
        spaces
        string ";"
        many (char '-')
        spaces
        return (RuleItem (Rule 0 name sequence)) -- I will reset the rulePriority using the order of rules later


--TODO clean this if you can
parseOperators =
    do
        string "operators:"
        spaces
        operatorSequences <- endBy parseOperator spaces
        return (OperatorsItem ((\(priority, associativity, symbol) ->
            (Operator symbol priority associativity)) <$> addPriority 0 operatorSequences))
    where
        addPriority::Int->[(Associativity, Sequence)]->[(Int, Associativity, Sequence)]
        addPriority p ((assoc, seq):rest) = (p, assoc, seq):addPriority (p+1) rest
        addPriority _ [] = []

parseOperator =
    do
        associativity <- option "l:" (string "r:")
        operator <- parseQuote
        return (if associativity == "l:" then LeftAssoc else RightAssoc, operator)

parseSeparator =
    do
        try (string "separator:")
        spaces
        separator<- parseQuote
        return (SeparatorItem separator)

parseLeft =
    do
        try (string "left:")
        spaces
        left<- parseSequence
        spaces
        string ";"
        return (LeftItem left)

parseRight =
    do
        try (string "right:")
        spaces
        right<- parseSequence
        spaces
        string ";"
        return (RightItem right)

-----------------------------

ident =
    do
        c<-letter
        cs<-many alphaNum
        return (c:cs)

parseQuote::Parser Sequence
parseQuote =
    do
        char '\''
        quotedString<-many (
            noneOf "'\\\n"
                <|> try (string "\\'" >> return '\'')
                <|> try (string "\\n" >> return '\n')
                <|> try (string "\\r" >> return '\r')
                <|> try (string "\\t" >> return '\t'))
        char '\''
        return (string2Sequence quotedString)

string2Sequence::String->Sequence
string2Sequence [] = []
string2Sequence ('_':rest) = WhiteSpace EmptyWS:string2Sequence rest
--TODO This allows for default space like " _ " (which doesn't make sense), then
-- doesn't even treat the _ properly.  This is a minor problem, I won't fix it now.
string2Sequence s | isSpace (head s) = WhiteSpace (WSString first):string2Sequence rest
    where (first, rest) = break (not . isSpaceOrUnderscore) s
string2Sequence s = TextMatch first Nothing:string2Sequence rest
    where (first, rest) = break isSpaceOrUnderscore s

isSpaceOrUnderscore::Char->Bool
isSpaceOrUnderscore c = isSpace c || c == '_'

many1WithSeparator::Parser a->Parser b->Parser [a]
many1WithSeparator x matchSeparator =
    do
        first<-x
        rest<-many $ try (do matchSeparator; val<-x; return val)
        return (first:rest)

data Count = One | ZeroOrMore | OneOrMore

parseSequence::Parser Sequence
parseSequence =
    do
        sequences <- many matchSequenceItem
        return (concat sequences)

data SequenceItem = TextItem String | SequenceItem Sequence | ExpressionItem Expression

matchSequenceItem =
    do
        item <- (
            exp2 SequenceItem matchAttribute
            <|> exp2 ExpressionItem matchLink
            <|> exp2 SequenceItem matchParen
            <|> exp2 ExpressionItem matchCharset
            <|> exp2 ExpressionItem matchSimpleCharsetChar
            <|> exp2 TextItem matchText)
        count <- option "" (string "*" <|> string "+" <|> string "?")
        return (case (count, item) of
            ("", ExpressionItem exp) -> [exp]
            ("", SequenceItem seq) -> seq
            ("", TextItem text) -> string2Sequence text
            ("*", ExpressionItem exp) -> [EQuote 0 [exp]]
            ("*", SequenceItem seq) -> [EQuote 0 seq]
            ("*", TextItem text) -> string2Sequence (init text) ++ [EQuote 0 [TextMatch [last text] Nothing]]
            ("+", ExpressionItem exp) -> [EQuote 1 [exp]]
            ("+", SequenceItem seq) -> [EQuote 1 seq]
            ("+", TextItem text) -> string2Sequence (init text) ++ [EQuote 1 [TextMatch [last text] Nothing]]
            ("?", ExpressionItem exp) -> [Option [exp]]
            ("?", SequenceItem seq) -> [Option seq]
            ("?", TextItem text) -> string2Sequence (init text) ++ [Option [TextMatch [last text] Nothing]])



exp2Seq::Parser Expression->Parser Sequence
exp2Seq x =
    do
        ret<-x
        return [ret]

exp2::(a->b)->Parser a->Parser b
exp2 f x =
    do
        ret<-x
        return (f ret)

matchAttribute =
    do
        char '@'
        name<-ident
        parseType<-option [Link "ident"] matchParen
        return ([Out [VStart name Nothing]] ++ parseType ++ [Out [VEnd]])

matchParen =
    do
        char '('
        sequence <- parseSequence
        char ')'
        return sequence

matchCharset =
    do
        char '['
        isInverse <- option False (char '^' >> return True)
        charTypes <- many (matchEscapedChar <|> try matchCharsetRange <|> matchCharsetChar)
        char ']'
        return (Character (CharSet isInverse charTypes) Nothing)

matchEscapedChar =
    do
        char '\\'
        escapedChar <- noneOf "]"
        return (unescape escapedChar)

matchCharsetChar =
    do
        char <- noneOf "]"
        return (SingleChar char)

matchCharsetRange =
    do
        char1 <- noneOf "]"
        char '-'
        char2 <- noneOf "]"
        return (CharRange char1 char2)

matchSimpleCharsetChar =
    do
        chartype <- (
            try (string "\\s" >> return Space)
            <|> try (string "\\d" >> return Digit)
            <|> try (string "\\w" >> return WordChar)
            )
        return (Character (CharSet False [chartype]) Nothing)

matchLink::Parser Expression
matchLink =
    do
        char '{'
        val<-ident
        char '}'
        return (Link val)

matchText::Parser String
matchText =
    do
        text<-many1 (noneOf ";*()[]+{}@?\\"
            <|> (try (string "\\@") >> return '@')
            <|> (try (string "\\?") >> return '?')
            <|> (try (string "\\*") >> return '*')
            <|> (try (string "\\+") >> return '+')
            <|> (try (string "\\;") >> return ';')
            <|> (try (string "\\(") >> return '(')
            <|> (try (string "\\)") >> return ')')
            <|> (try (string "\\[") >> return '[')
            <|> (try (string "\\]") >> return ']')
            <|> (try (string "\\{") >> return '{')
            <|> (try (string "\\}") >> return '}'))
        return text

string2CharTypes::String->[CharType]
string2CharTypes ('\\':'s':rest) = Space:string2CharTypes rest
string2CharTypes ('\\':'d':rest) = Digit:string2CharTypes rest
string2CharTypes ('\\':'n':rest) = SingleChar '\n':string2CharTypes rest
string2CharTypes ('\\':'r':rest) = SingleChar '\r':string2CharTypes rest
string2CharTypes ('\\':'t':rest) = SingleChar '\t':string2CharTypes rest
string2CharTypes ('\\':c:_) = error ("Missing escape char in string2CharTypes " ++ [c])
string2CharTypes (c:rest) = SingleChar c:string2CharTypes rest
string2CharTypes [] = []

unescape::Char->CharType
unescape 's' = Space
unescape 'd' = Digit
unescape 'n' = SingleChar '\n'
unescape 'r' = SingleChar '\r'
unescape 't' = SingleChar '\t'
unescape '-' = SingleChar '-'
unescape c = error ("Missing escape char in string2CharTypes " ++ [c])
