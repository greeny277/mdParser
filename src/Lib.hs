module Lib where

import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim
import Text.Parsec.Char
import Control.Monad (void)
import Data.List (intercalate)

-- Some test strings
emptyTestString = ""
testString = "## Sub-heading\n\
\\n\
\Paragraphs are separated \n\
\by a blank line. \n\
\\n\
\![image](fff.png)\n\
\\n\
\[link](www.google.com)\n\
\Two spaces at the end of a line  \n\
\produces a line break.\n\
\\n\
\Text attributes _italic_,\n\
\**bold**, `monospace`.\n\
\\n\
\Horizontal rule:\n\
\\n\
\------\n\
\\n\
\* Apple\n\
\* Banana\n\
\\n\
\> Very blockquote > < &\n\
\> Much more blockquote"

listTest = "1. Fooo\n2. Baaar\n\nfooooobar"
imageTest = "![image](fff.png)"


-- Start the parsing procedure
startParsing :: IO ()
startParsing = do
        let r =  parse parseMD "" testString
        case r of
            Left _  -> putStrLn "Parsing went wrong"
            Right h -> putStrLn h

-- This part can be extended, for example by adding a function for bullet lists. I would argue, that it makes sense to
-- test at last for paragraphs.
parseMD :: Parsec String () String
parseMD = intercalate "\n"  <$> ((parseHeader <|> parseList <|> parseHorizontal <|> parseBlockquote <|> parseParagraph <|> many anyChar) `sepBy` many1 newline)

-- Is the current line a header
parseHeader :: Parsec String () String
parseHeader = do
        lookAhead (char '#')
        sectionLevel <- getHeaderLevel
        cs <- spaces >> many1 (noneOf "\n")
        return ("<h" ++ show sectionLevel ++ ">" ++ cs ++ "</h" ++ show sectionLevel ++ ">")

-- Ask for the header level
getHeaderLevel :: Parsec String () Int
getHeaderLevel = (do char '#'; getHeaderLevel >>= (\n -> return (n+1))) <|> return 0

-- Parse lists
parseList :: Parsec String () String
parseList = do
        listType <- (lookAhead digit >> return "ol") <|> (lookAhead (char '*') >> return "ul")
        let
            startTag   = "<" ++ listType ++ ">"
            endTag   = "</" ++ listType ++ ">"
            in do
                  items <- intercalate "\n" <$> many1 (parseListItem listType)
                  return $ startTag ++ "\n" ++ init items ++ "\n" ++ endTag

-- Parse a list item
parseListItem :: String -> Parsec String () String
parseListItem listType =
                if listType == "ol"
                    then digit >> char '.' >> spaces >> parseListContent listType
                    else char '*' >> spaces >> parseListContent listType

-- Parse content of an item an call `parseListItem` again
parseListContent :: String -> Parsec String () String
parseListContent listType  =
        let
            startTag = "<li>"
            endTag   = "</li>"
            in
                try (do
                    content <- spaces >> many1 (noneOf "\n")
                    restList <- parseUntilEmptyLine $ parseListItem listType
                    return $ (startTag ++ content ++ endTag) ++ ('\n':restList)
                    )

parseUntilEmptyLine :: Parsec String () String -> Parsec String () String
parseUntilEmptyLine p =
        try (do newline; lookAhead newline; return "") <|> try ((newline <* eof) >> return "") <|> (do lookAhead newline; newline; p) <|> try (do r <- p; (r++) <$> parseUntilEmptyLine p) <|> return ""

-- Parse a paragraph
parseParagraph :: Parsec String () String
parseParagraph = do
        let startTag = "<p>"
            endTag   = "</p>"
        content <- (newline >> readParInput) <|> readParInput
        return $ startTag ++ content ++ endTag

-- Parse content of a paragraph; search for attributes and the end of
-- a parapgraph.
readParInput :: Parsec String () String
readParInput = do
        content <- concat <$> many1 (many1 (noneOf "\n*`_ ![") <|> parseAttribute <|> parseWhitespace <|> try parseImage <|> try parseLink)
        nextLine <- parseUntilEmptyLine readParInput
        return (content ++ nextLine)

parseLink :: Parsec String () String
parseLink =
        do
            string "["
            linkName <- many (noneOf "]")
            char ']'
            char '('
            link <- many (noneOf ")")
            char ')'
            return $ "<a href = \"" ++ linkName ++ "\" src=\"" ++ link ++ "\"</a>"

parseImage :: Parsec String () String
parseImage =
        do
            string "!["
            imageName <- many (noneOf "]")
            char ']'
            char '('
            imageLink <- many (noneOf ")")
            char ')'
            return $ "<img alt = \"" ++ imageName ++ "\" src=\"" ++ imageLink ++ "\" />"

-- Two whitespaces at the end of a line indicate a linebreak in html
parseWhitespace :: Parsec String () String
parseWhitespace =
        let checkForBr = char ' ' >> char ' ' >> newline
            in try (checkForBr >> return "< /br>\n") <|> (char ' ' >> return " ")

-- Parse any of the three given attributes
parseAttribute :: Parsec String () String
parseAttribute = parseAttribute' "strong" <|> parseAttribute' "em" <|> parseAttribute' "code"

parseAttribute' :: String -> Parsec String () String
parseAttribute' tag = do
        let startTag = "<"  ++ tag ++ ">"
            endTag   = "</" ++ tag ++ ">"
            specialChar = helper tag

        many1 (char specialChar)
        boldedText <- many1 (noneOf (show specialChar))
        many1 (char specialChar)
        return $ startTag ++ boldedText ++ endTag
        where helper :: String -> Char
              helper s
                | s == "strong" = '*'
                | s == "em" = '_'
                | otherwise      = '`'

parseHorizontal :: Parsec String () String
parseHorizontal = many1 (char '-') >> lookAhead newline >> return "<hr />"

-- Parse a blockquote. Use email style characters for blockquoting.
parseBlockquote :: Parsec String () String
parseBlockquote = do
        let
            startTag = "<blockquote>\n<p>"
            endTag   = "</p>\n</blockquote>"
        lookAhead (char '>')
        blockquote <- parseUntilEmptyLine parseBlockquoteContent
        return (startTag ++ blockquote ++ endTag)

parseBlockquoteContent :: Parsec String () String
parseBlockquoteContent = do
        char '>' >> spaces
        (++ " ") . concat <$> many1 (many1 (noneOf "<>&\n")
                <|> (char '>' >> return "&gt;")
                <|> (char '<' >> return "&lt;")
                <|> (char '&' >> return "&amp;"))
