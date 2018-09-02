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
\ Paragraphs are separated \n\
\ by a blank line. \n\
\\n\
\\n\
\Two spaces at the end of a line  \n\
\produces a line break.\n\
\\n\
\Text attributes _italic_,\n\
\**bold**, `monospace`.\n\
\\n\
\Horizontal rule:\n\
\\n\
\------\n\
\* Apple\n\
\* Banana"

listTest = "1. Fooo\n2. Baaar"


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
parseMD = intercalate "\n"  <$> ((parseHeader <|> parseList <|> parseHorizontal <|> parseParagraph <|> many anyChar) `sepBy` many1 newline)

-- Is the current line a header
parseHeader :: Parsec String () String
parseHeader = do
        lookAhead (char '#')
        sectionLevel <- getHeaderLevel
        cs <- many1 (noneOf "\n")
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
                  items <- intercalate "\n" <$> parseListItem `sepBy` newline
                  return $ startTag ++ "\n" ++ items ++ "\n" ++ endTag

parseListItem :: Parsec String () String
parseListItem =
        let
            startTag   = "<li>"
            endTag   = "</li>"
            in do
                (char '*' <|> (digit >> char '.')) >> spaces
                content <- many (noneOf "\n")
                return $ startTag ++ content ++ endTag


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
        content <- concat <$> many1 (many1 (noneOf "\n*`_ ") <|> parseAttribute <|> parseWhitespace)
        nextLine <- try (do newline; lookAhead newline; return "") <|> (do lookAhead newline; newline; readParInput >>= (\tmp -> return ('\n':tmp))) <|> return ""
        return (content ++ nextLine)

parseImage :: Parsec String () String
parseImage =
        try (do
            string "!["
            imageName <- many (noneOf "]")
            char ']'
            char '('
            imageLink <- many (noneOf ")")
            char ')'
            return $ "<img alt = \"" ++ imageName ++ "\" src=\"" ++ imageLink ++ "\" />"
            )
-- Two whitespaces at the end of a line indicate a linebreak in html
parseWhitespace :: Parsec String () String
parseWhitespace =
        let checkForBr = char ' ' >> char ' ' >> newline
            in try (checkForBr >> return "< /br>") <|> (char ' ' >> return " ")

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
