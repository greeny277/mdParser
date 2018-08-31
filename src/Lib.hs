module Lib where

import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim
import Text.Parsec.Char
import Control.Monad (void)

-- Some test strings
emptyTestString = ""
testString = "Subsection"
javaTestString = "## Sub-heading\n### Sub-sub-Heading\n\n**paragpraph**  \nfoooo";


-- Start the parsing procedure
startParsing :: IO ()
startParsing = do
        let r =  parse parseMD "" javaTestString
        case r of
            Left _  -> putStrLn "Parsing went wrong"
            Right h -> putStrLn h

-- This part can be extended, for example by adding a function for bullet lists. I would argue, that it makes sense to
-- test at last for paragraphs.
parseMD :: Parsec String () String
parseMD = concat <$> ((parseHeader <|> parseParagraph) `sepBy` newline)


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
        nextLine <- try (do char '\n'; char '\n'; newline; return "") <|> (do lookAhead (char '\n'); newline; readParInput >>= (\tmp -> return ('\n':tmp))) <|> return ""
        return (content ++ nextLine)



parseWhitespace :: Parsec String () String
parseWhitespace =
        let checkForBr = char ' ' >> char ' ' >> newline
            in (checkForBr >> return "< /br>") <|> (char ' ' >> return " ")


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

