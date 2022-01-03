module Exam where

import Data.Char
import Data.Maybe
import Debug.Trace
  
type Name = String

type Attributes = [(Name, String)]

data XML = Null | Text String | Element Name Attributes [XML]
         deriving (Eq, Show)

type Stack = [XML]

-- TOTAL: 23/30 (77%)
-----------------------------------------------------------------------
-- Some useful show/print functions

-- The 'show' function for XML objects
showXML :: XML -> String
showXML (Text t)
  = t
showXML (Element n as es)
  = "<" ++ n ++ showAtts as ++ ">" ++ concatMap showXML es ++ "</" ++ n ++ ">"
  where
    showAtts as = concatMap showAtt as
    showAtt (n, v) = " " ++ n ++ "=" ++ "\"" ++ v ++ "\""

-- The 'show' function for lists of XML objects
showXMLs :: [XML] -> String
showXMLs
  = concatMap showXML

-- Prints an XML object to the terminal
printXML :: XML -> IO()
printXML 
  = putStrLn . showXML

-- Prints a list of XML objects to the terminal (useful for testing the
-- output from expandXML')
printXMLs :: [XML] -> IO()
printXMLs
  = mapM_ printXML

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- Part I: 11/12

-- 1/1
skipSpace :: String -> String
skipSpace s
  = dropWhile isSpace s -- * Could have used extensionality to simplify further

-- 1/2: No need to get attributes from children
-- getAttribute :: String -> XML -> String
-- getAttribute s (Element _ as xmls)
--   = fromMaybe "" (lookup s as) ++ (concatMap (getAttribute s) xmls)
-- getAttribute _ _
--   = ""

-- * getAttribute does not need to get the attributes from its children
getAttribute :: String -> XML -> String
getAttribute s (Element _ as _)
  = fromMaybe "" (lookup s as)
getAttribute _ _
  = ""

-- 2/2
-- getChildren :: String -> XML -> [XML]
-- getChildren s (Element _ _ cs)
--   = getChildren' s cs
--     where
--       getChildren' :: String -> [XML] -> [XML]
--       getChildren' _ []
--         = []
--       getChildren' s (xml@(Element name _ _) : xmls)
--         | s == name = xml : getChildren' s xmls
--       getChildren' s (_ : xmls)
--         = getChildren' s xmls

-- * Can be done without a helper function
getChildren :: String -> XML -> [XML]
getChildren s (Element _ _ (x@(Element n _ _) : xs))
  | s == n = x : getChildren s (Element "" [] xs)
  | otherwise = getChildren s (Element "" [] xs)
getChildren _ _
  = []

-- getChildren :: String -> XML -> [XML]
-- getChildren s (Element _ _ cs)
--   = filter ((==s) . getName . isElement) cs
--     where
--       isElement :: XML -> Bool
--       isElement (Element _ _ _)
--         = True
--       isElement _
--         = False
--       getName :: XML -> String 
--       -- Pre: XML is an element
--       getName (Element name _ _)
--         = name
-- getChildren _ _
--   = []

-- 2/2
getChild :: String -> XML -> XML
getChild s xml
  | null child = Text ""
  | otherwise  = head child
    where
      child = getChildren s xml 

-- 1/1
addChild :: XML -> XML -> XML
-- Pre: the second argument is an Element
addChild c (Element name as cs)
  = Element name as (cs ++ [c])

-- 4/4 
getValue :: XML -> XML
getValue xml 
  = Text (getValue' xml) 
    where
      getValue' :: XML -> String
      getValue' (Element _ _ cs)
        = concatMap getValue' cs
      getValue' (Text x)
        = x
      getValue' _
        = ""

-------------------------------------------------------------------------
-- Part II: 11/12

-- Parses an element/attribute name
parseName :: String -> (Name, String)
parseName []
  = error "Error: attempt to read empty name"
parseName s@(c : cs)
  | isAlpha c = break (not . isNameChar) s
  | otherwise = error ("parseName error: name " ++ show s ++
                      " must begin with a letter")
  where
    isNameChar c = isAlpha c || isDigit c || elem c "-."

sentinel :: XML
sentinel 
  = Element "" [] []

-- 1/1
addText :: String -> Stack -> Stack
-- Pre: There is at least one Element on the stack
addText str (e : es)
  = (addChild (Text str) e) : es

-- 1/1
popAndAdd :: Stack -> Stack
-- Pre: There are at least two Elements on the stack
popAndAdd (e : e' : es)
  = (addChild e e') : es

-- 2/3: parseName requires the string to not have any spaces before and the two break lines can be combined into one
-- parseAttributes :: String -> (Attributes, String)
-- -- Pre: The XML attributes string is well-formed
-- parseAttributes  s
--   | c == '>' = ([], s')
--   | otherwise = ((name, v) : as, cs)
--     where
--       (c : s') = skipSpace s 
--       (name, s'') = parseName s
--       (_, (_ : s''')) = break ('\"'==) s''
--       (v, (_ : s'''')) = break ('\"'==) s'''
--       (as, cs)         = parseAttributes (skipSpace s'''')

-- * using skipSpace for parseName
parseAttributes :: String -> (Attributes, String)
-- Pre: The XML attributes string is well-formed
parseAttributes  s
  | c == '>' = ([], cs)
  | otherwise = ((name, v) : as, xs)
    where
      s'@(c : cs) = skipSpace s 
      (name, rem) = parseName s'
      (v, (_ : rem')) = break ('\"'==) ((tail . dropWhile ('\"'/=)) rem)
      (as, xs)        = parseAttributes rem' 


firstChild :: XML -> XML
-- Pre: XML must be an element
firstChild (Element _ _ (xml : _))
  = xml

parse :: String -> XML
-- Pre: The XML string is well-formed
parse s
  = parse' (skipSpace s) [sentinel]

-- 7/7: dropWhile should be used rather than break with (_, (_ : s')) and skipSpace is not required for parseAttributes
-- (Although with my implementation of parseAttributes the skipSpace was required)
-- parse' :: String -> Stack -> XML
-- parse' "" (xml : _)
--   = firstChild xml
-- parse' str@(c : c' : s) stack
--   | c == '<' && c' == '/' = parse' s' (popAndAdd stack)
--   | c == '<'              = parse' s''' ((Element ename as []) : stack)  
--   | otherwise             = parse' rem (addText text stack)
--     where
--       (_, (_ : s')) = break ('>'==) s
--       (ename, s'')  = parseName (c' : s)
--       (as, s''')    = parseAttributes (skipSpace s'')
--       (text, rem)   = break ('<'==) str

-- * We can use pattern matching rather than adding a firstChild helper function 
parse' :: String -> Stack -> XML
parse' "" ((Element _ _ (x : _)) : _)
  = x 
parse' str@(c : c' : s) stack
  | c == '<' && c' == '/' = parse' s' (popAndAdd stack)
  | c == '<'              = parse' s''' ((Element n as []) : stack)  
  | otherwise             = parse' rem (addText text stack)
    where
      s'          = (tail . dropWhile ('>'/=)) s
      (n, s'')    = parseName (c' : s)
      (as, s''')  = parseAttributes s''
      (text, rem) = break ('<'==) str


-------------------------------------------------------------------------
-- Part III: 1/6

type Context = XML

type XSL = XML

-- Parses XSL and XML source documents and transforms the latter using the
-- former. The output is written to the given file (String).
-- Example use:
--   output "out.html" filmsXSL films
-- To render output.html in a browser, type this at the Linux prompt:
--   firefox output.html &
output :: String -> XML -> XML -> IO()
output file xsl source
  = writeFile file (showXMLs (expandXSL xsl source))

-- 1/6: Some progress towards correct solution
expandXSL :: XSL -> XML -> [XML]
expandXSL xsl source 
  = expandXSL' root xsl
  where
    root = Element "/" [] [source]

expandXSL' :: Context -> XSL -> [XML]
expandXSL' context x@(Element "value-of" _ _)
  = [expandVO (getAttribute "select" x) context]
expandXSL' context x@(Element "for-each" _ es)
  = concat [concatMap (expandXSL' c) es | c <- expandFE (getAttribute "select" x) [context]]
expandXSL' context x@(Element n as xs)
  = [Element n as (concatMap (expandXSL' context) xs)]
expandXSL' _ x
  = [x]

expandVO :: String -> Context -> XML
expandVO "" context
  = getValue context
expandVO xpath@(c : cs) context
  | c == '.' && null xpath' = expandVO "" context
  | c == '.'                = expandVO (tail xpath') context
  | c == '@' = Text (getAttribute cs context)
  | null xpath' = expandVO "" (getChild step context)
  | otherwise = expandVO (tail xpath') (getChild step context) 
    where
      (step, xpath') = break (=='/') xpath

expandFE :: String -> [Context] -> [Context] 
expandFE "" contexts
  = contexts 
expandFE xpath@(c : cs) contexts
  | c == '.' && null xpath' = expandFE "" contexts
  | c == '.'                = expandFE (tail xpath') contexts
  | null xpath' = concatMap (getChildren step) contexts
  | otherwise = expandFE (tail xpath') (concatMap (getChildren step) contexts)
    where
      (step, xpath') = break (=='/') xpath


-- findValue :: String -> Context -> String
-- findValue xpath context
--   | (not . null) step && (((=='@') . head) step) = getAttribute (tail step) context -- +0.5
--   | null xpath' = (\(Element name _ _) -> name) context 
--   | otherwise   = concatMap (findValue xpath') (getChildren step context)
--     where
--       (step, xpath') = break ('/'==) xpath -- +0.5
-- 
-- findXPath :: String -> Context -> [XML]
-- findXPath xpath context 
--   | step == "." = [Text ""]
--   | (not . null) step && head step  == '@' = [Text (getAttribute (tail step) context)]
--   | null xpath' = trace ("step: " ++ show step) getChildren step context
--   | otherwise   = concatMap (findXPath xpath') (getChildren step context)
--     where
--       (step, xpath') = break ('/'==) xpath
-- 
-- expandXSL' :: Context -> XSL -> [XML]
-- expandXSL' context (Element "value-of" [(_, xpath)] _)
--   = [Text s]
--     where
--       s = findValue xpath context

-------------------------------------------------------------------------
-- Test data for Parts I and II

-- Simple test cases (no whitespace)
s1, s2, s3 :: String
s1
  = "<a>A</a>"
s2 
  = "<a x=\"1\"><b>A</b><b>B</b></a>"
s3
  = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>"

-- Parsed versions of the above
x1, x2, x3 :: XML
x1
  = Element "a" [] [Text "A"]
x2
  = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3
  = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]

casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]

-- Films mark-up of Figure 1
films :: String
films
  = "<filmlist>\n\
    \  <film title = \"Rear Window\">\n\
    \    <director>Alfred Hitchcock</director>\n\
    \    <composer>Franz Waxman</composer>\n\
    \    <year>1954</year>\n\
    \  </film>\n\
    \  <film   title =  \"2001: A Space Odyssey\">\n\
    \    <director>Stanley Kubrick</director>\n\
    \    <composer>Richard Strauss</composer>\n\
    \    <composer>Gyorgy Ligeti</composer>\n\
    \    <composer>Johann Strauss</composer>\n\
    \    <year>1968</year>\n\
    \  </film>\n\
    \  <film title=\"Lawrence of Arabia\"  >\n\
    \    <duration>228</duration>\n\
    \    <director>David Lean</director>\n\
    \    <composer>Maurice Jarre</composer>\n\
    \  </film>\n\
    \</filmlist>\n\n\n"

-- Parsed version of films ('parse films'), suitably formatted
filmsParsed :: XML
filmsParsed
  = Element "filmlist" 
            [] 
            [Text "\n  ",
             Element "film" [("title","Rear Window")]
                            [Text "\n    ",
                             Element "director" [] [Text "Alfred Hitchcock"],
                             Text "\n    ",
                             Element "composer" [] [Text "Franz Waxman"],
                             Text "\n    ",
                             Element "year" [] [Text "1954"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","2001: A Space Odyssey")] 
                            [Text "\n    ",
                             Element "director" [] [Text "Stanley Kubrick"],
                             Text "\n    ",
                             Element "composer" [] [Text "Richard Strauss"],
                             Text "\n    ",
                             Element "composer" [] [Text "Gyorgy Ligeti"],
                             Text "\n    ",
                             Element "composer" [] [Text "Johann Strauss"],
                             Text "\n    ",
                             Element "year" [] [Text "1968"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","Lawrence of Arabia")] 
                            [Text "\n    ",
                             Element "duration" [] [Text "228"],
                             Text "\n    ",
                             Element "director" [] [Text "David Lean"],
                             Text "\n    ",
                             Element "composer" [] [Text "Maurice Jarre"],
                             Text "\n  "],
             Text "\n"]

-------------------------------------------------------------------------
-- XSL tests

-- value-of test cases
xsl1, xsl2, xsl3, xsl4, xsl5, xsl6, xsl7, 
  xsl8, xsl9 :: String
xsl1
  = "<value-of select = \"a/b/c\"></value-of>"
xsl2
  = "<value-of select = \"a/b\"></value-of>"
xsl3
  = "<value-of select = \"a/b/d\"></value-of>"
xsl4
  = "<value-of select = \"a/b/c/@att\"></value-of>"
xsl5
  = "<value-of select = \"./a/./b/c/./.\"></value-of>"
xsl6
  = "<t1><t2>Preamble</t2><t3><value-of select = \"a/b/c\"></value-of></t3></t1>"

-- for-each test cases
xsl7
  = "<for-each select=\"a/b/c\"><value-of select=\"./@att\"></value-of>\
    \</for-each>" 
xsl8
  = "<for-each select=\"a/b\"><t><value-of select=\"c\"></value-of></t>\
    \</for-each>" 
xsl9
  = "<for-each select=\"a/b\"><t1><value-of select=\"absent\"></value-of>\
    \</t1></for-each>"
        
-- Parsed versions of the above
xsl1Parsed, xsl2Parsed, xsl3Parsed, xsl4Parsed, xsl5Parsed,
  xsl6Parsed, xsl7Parsed, xsl8Parsed, xsl9Parsed :: XML
xsl1Parsed
  = Element "value-of" [("select","a/b/c")] []
xsl2Parsed
  = Element "value-of" [("select","a/b")] []
xsl3Parsed
  = Element "value-of" [("select","a/b/d")] []
xsl4Parsed
  = Element "value-of" [("select","a/b/c/@att")] []
xsl5Parsed
  = Element "value-of" [("select","./a/./b/c/./.")] []
xsl6Parsed
  = Element "t1" 
            [] 
            [Element "t2" [] [Text "Preamble"],
             Element "t3" [] [Element "value-of" [("select","a/b/c")] []]]

xsl7Parsed
  = Element "for-each" 
            [("select","a/b/c")] 
            [Element "value-of" [("select","./@att")] []]
xsl8Parsed
  = Element "for-each" 
            [("select","a/b")] 
            [Element "t" [] [Element "value-of" [("select","c")] []]]
xsl9Parsed
  = Element "for-each" 
            [("select","a/b")] 
            [Element "t1" [] [Element "value-of" [("select","absent")] []]]

-- XSL template for building a films summary (example from spec.)
filmsXSL :: String
filmsXSL
  = "<html>\n\
    \<body>\n\
    \  <h2>Film List</h2>\n\
    \  <table border=\"1\">\n\
    \    <tr>\n\
    \      <th align=\"left\">Title</th>\n\
    \      <th align=\"left\">Director</th>\n\
    \      <th align=\"left\">Principal composer</th>\n\
    \    </tr>\n\
    \    <for-each select=\"filmlist/film\">\n\
    \      <tr>\n\
    \        <td><value-of select=\"@title\"></value-of></td>\n\
    \        <td><value-of select=\"director\"></value-of></td>\n\
    \        <td><value-of select=\"composer\"></value-of></td>\n\
    \      </tr>\n\
    \    </for-each>\n\
    \  </table>\n\
    \</body>\n\
    \</html>"

-- XSL template for building a list of composers (example from spec.)
composersXSL :: String
composersXSL
  = "<for-each select=\"filmlist/film\">\
      \<h2><value-of select=\"@title\"></value-of> composers</h2>\
      \<ul>\
      \<for-each select=\"composer\">\
        \<li><value-of select=\".\"></value-of></li>\
      \</for-each>\
      \</ul>\
    \</for-each>"
