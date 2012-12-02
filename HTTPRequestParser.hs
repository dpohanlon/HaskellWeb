module HTTPRequestParser
(
    HTTPRequest(..),
    parseHTTP
) where

import Text.ParserCombinators.Parsec

data HTTPRequest = HTTPRequest {
    reqSite :: String,
    reqURL :: String
} deriving (Eq, Show)

httpReq = endBy line eol

line = getURL
    <|> getHost
    <?> "Request Header"

getURL = do
    string "GET "
    url <- many $ noneOf ",\n\r "
    string " HTTP/"
    httpVer <- many $ noneOf ",\n\r"
    return url

getHost = do
    string "Host: "
    h <- many $ noneOf ",\n\r"
    return h

eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "EOL"

--eol = string "\n\r"

httpInfo :: [String] -> HTTPRequest
httpInfo info = HTTPRequest{ reqSite = info !! 1,
                             reqURL =  info !! 0 }

--parseHTTP :: String -> Maybe HTTPRequest
--parseHTTP input = do
--  case parse httpReq "HaskellWeb" input of
--      Left _ -> Nothing
--      Right r -> Just $ httpInfo r

parseHTTP :: String -> HTTPRequest
parseHTTP input = do
    case parse httpReq "HaskellWeb" input of
        Right r -> httpInfo r
        Left _ -> httpInfo [" "," "] -- Please fix me!

main = do
    c <- getContents
    print $ parseHTTP c
    