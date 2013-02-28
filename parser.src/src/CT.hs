{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (readFile, writeFile)
import Text.XML
import Text.Hamlet.XML
import Data.Map (empty, fromList, toList)
import qualified Data.Map as Map

main :: IO ()
main = do
    -- readFile will throw any parse errors as runtime exceptions
    -- def uses the default settings
    Document prologue root epilogue <- readFile def "input.xml"

    -- root is the root element of the document, let's modify it
    let root' = transform root

    -- And now we write out. Let's indent our output
    writeFile def
        { rsPretty = True
        } "output.html" $ Document prologue root' epilogue

-- We'll turn out <document> into an XHTML document
transform :: Element -> Element
transform (Element _name attrs children) = Element "html" empty [xml|
<head>
    <title>
        $maybe title <- Map.lookup "title" attrs
            \#{title}
        $nothing
            Untitled Document
<body>
    $forall child <- children
        ^{goNode child}
|]

goNode :: Node -> [Node]
goNode (NodeElement e) = [NodeElement $ goElem e]
goNode (NodeContent t) = [NodeContent t]
goNode (NodeComment _) = [] -- hide comments
goNode (NodeInstruction _) = [] -- and hide processing instructions too

-- convert each source element to its XHTML equivalent
goElem :: Element -> Element
goElem (Element "para" attrs children) =
    Element "p" attrs $ concatMap goNode children
goElem (Element "em" attrs children) =
    Element "i" attrs $ concatMap goNode children
goElem (Element "strong" attrs children) =
    Element "b" attrs $ concatMap goNode children
goElem (Element "image" attrs _children) =
    Element "img" (fromList $ map fixAttr $ toList attrs) [] -- images can't have children
  where
    fixAttr ("href", value) = ("src", value)
    fixAttr x = x
goElem (Element name attrs children) =
    -- don't know what to do, just pass it through...
    Element name attrs $ concatMap goNode children
