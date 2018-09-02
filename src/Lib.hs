{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Monad (void)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim
import Text.Parsec.Char

-- Start the parsing procedure
startParsing :: IO ()
startParsing = do
        content <- T.getContents
        let r =  parse parseMD "" content
        case r of
            Left _  -> T.putStrLn "Parsing went wrong"
            Right h -> T.putStrLn h

-- This part can be extended, for example by adding a function for bullet lists. I would argue, that it makes sense to
-- test at last for paragraphs.
parseMD :: Parsec T.Text () T.Text
parseMD = T.intercalate "\n"  <$>
            ((parseHeader
          <|> parseList
          <|> parseHorizontal
          <|> parseBlockquote
          <|> parseParagraph
          <|> T.pack <$> many1 anyChar)
            `sepEndBy` many1 (newline *> skipMany (space <|> tab)))

-- Is the current line a header
parseHeader :: Parsec T.Text () T.Text
parseHeader = do
        lookAhead (char '#')
        sectionLevel <- T.pack . show <$> getHeaderLevel
        cs <- T.pack <$> (spaces *> many1 (noneOf "\n"))
        return (
              "<h" `T.append` sectionLevel `T.append` ">"
              `T.append` cs `T.append`
              "</h" `T.append` sectionLevel `T.append` ">")

-- Ask for the header level
getHeaderLevel :: Parsec T.Text () Int
getHeaderLevel = (do char '#'; getHeaderLevel >>= (\n -> return (n+1))) <|> return 0

-- Parse lists
parseList :: Parsec T.Text () T.Text
parseList = do
        listType <- (lookAhead digit *> return "ol") <|> (lookAhead (char '*') *> return "ul")
        let
            startTag   = "<" `T.append` listType `T.append` ">"
            endTag   = "</" `T.append` listType `T.append` ">"
            in do
                  items <- T.intercalate "\n" <$> many1 (parseListItem listType)
                  return $ startTag `T.append` "\n"
                     `T.append` T.init items `T.append` "\n" `T.append` endTag

-- Parse a list item
parseListItem :: T.Text -> Parsec T.Text () T.Text
parseListItem listType =
                if listType == "ol"
                    then digit *> char '.' *> spaces *> parseListContent listType
                    else char '*' *> spaces *> parseListContent listType

-- Parse content of an item an call `parseListItem` again
parseListContent :: T.Text -> Parsec T.Text () T.Text
parseListContent listType  =
        let
            startTag = "<li>"
            endTag   = "</li>"
            in
                try (do
                    content <- T.pack <$> (spaces *> many1 (noneOf "\n"))
                    restList <- parseUntilEmptyLine $ parseListItem listType
                    return $ (startTag `T.append` content `T.append` endTag)
                        `T.append` ('\n' `T.cons` restList)
                    )

parseUntilEmptyLine :: Parsec T.Text () T.Text -> Parsec T.Text () T.Text
parseUntilEmptyLine p =
        try (do newline; lookAhead newline; return T.empty)
                <|> try ((newline <* eof) *> return T.empty)
                <|> (do lookAhead newline; newline; skipMany (space <|> tab); p)
                <|> try (do r <- p; (r `T.append`) <$> parseUntilEmptyLine p)
                <|> return T.empty

-- Parse a paragraph
parseParagraph :: Parsec T.Text () T.Text
parseParagraph = do
        let startTag = "<p>"
            endTag   = "</p>"
        content <- (newline *> readParInput) <|> readParInput
        return $ startTag `T.append` content `T.append` endTag

-- Parse content of a paragraph; search for attributes and the end of
-- a parapgraph.
readParInput :: Parsec T.Text () T.Text
readParInput = do
        content <- T.concat
            <$> many1 (T.pack <$> many1 (noneOf "\n*`_ ![")
            <|> parseAttribute
            <|> parseWhitespace
            <|> try parseImage
            <|> try parseLink)
        nextLine <- parseUntilEmptyLine readParInput
        return (content `T.append` nextLine)

parseLink :: Parsec T.Text () T.Text
parseLink =
        do
            string "["
            linkName <- T.pack <$> many (noneOf "]")
            char ']'
            char '('
            link <- T.pack <$> many (noneOf ")")
            char ')'
            return $ "<a href = \"" `T.append` linkName `T.append` "\" src=\"" `T.append` link `T.append` "\"</a>"

parseImage :: Parsec T.Text () T.Text
parseImage =
        do
            string "!["
            imageName <- T.pack <$> many (noneOf "]")
            char ']'
            char '('
            imageLink <- T.pack <$> many (noneOf ")")
            char ')'
            return $ "<img alt = \"" `T.append` imageName `T.append` "\" src=\"" `T.append` imageLink `T.append` "\" />"

-- Two whitespaces at the end of a line indicate a linebreak in html
parseWhitespace :: Parsec T.Text () T.Text
parseWhitespace =
        let checkForBr = char ' ' *> char ' ' *> newline
            in try (checkForBr *> return "< /br>\n") <|> (char ' ' *> return " ")

-- Parse any of the three given attributes
parseAttribute :: Parsec T.Text () T.Text
parseAttribute = parseAttribute' "strong" <|> parseAttribute' "em" <|> parseAttribute' "code"

parseAttribute' :: T.Text -> Parsec T.Text () T.Text
parseAttribute' tag = do
        let startTag = "<"  `T.append` tag `T.append` ">"
            endTag   = "</" `T.append` tag `T.append` ">"
            specialChar = helper tag

        many1 (char specialChar)
        boldedText <- T.pack <$> many1 (noneOf (show specialChar))
        many1 (char specialChar)
        return $ startTag `T.append` boldedText `T.append` endTag
        where helper :: T.Text -> Char
              helper s
                | s == "strong" = '*'
                | s == "em" = '_'
                | otherwise      = '`'

parseHorizontal :: Parsec T.Text () T.Text
parseHorizontal = many1 (char '-') *> lookAhead newline *> return "<hr />"

-- Parse a blockquote. Use email style characters for blockquoting.
parseBlockquote :: Parsec T.Text () T.Text
parseBlockquote = do
        let
            startTag = "<blockquote>\n<p>"
            endTag   = "</p>\n</blockquote>"
        lookAhead (char '>')
        blockquote <- parseUntilEmptyLine parseBlockquoteContent
        return (startTag `T.append` blockquote `T.append` endTag)

parseBlockquoteContent :: Parsec T.Text () T.Text
parseBlockquoteContent = do
        char '>' *> spaces
        (`T.snoc` ' ') . T.pack . concat <$> many1 (many1 (noneOf "<>&\n")
                <|> (char '>' *> return "&gt;")
                <|> (char '<' *> return "&lt;")
                <|> (char '&' *> return "&amp;"))
