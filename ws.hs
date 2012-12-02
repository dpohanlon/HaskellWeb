{-

Simple static HTTP web server in Haskell. Only responds to requests of the form
"GET /foo/bar/baz.html HTTP/1.1\r"

Logs in /var/www/logs/server.log as TIME/DATE HOST REQUEST

Fully daemonized - control with {start, stop, restart}

TODO:
    Configuration file - set log settings and file paths
    Respond to more HTTP requests (HEAD, etc)
    Syntax
    Server statistics

-}

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Data.Maybe
import GHC.IO.Handle (hClose)
import System.Environment (getArgs)
import System.FilePath.Posix (takeExtension)
import System.Posix.Files (fileExist)
import System.IO (hSetBuffering, hGetLine, hGetContents, hPutStr, hPutStrLn, hFileSize, openFile,
                  IOMode(ReadMode), FilePath, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import Text.Regex.Posix
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import System.Posix.Daemonize
import System.Posix.Syslog (syslog, Priority(..))
import HTTPRequestParser
import qualified Data.ByteString.Lazy as ByteS

main :: IO ()
main = serviced runHaskellWeb

runHaskellWeb :: CreateDaemon Socket -- privilegedAction (IO a) -> CreateDaemon (a)
runHaskellWeb = simpleDaemon { program          = haskellWeb,
                               privilegedAction = bindPort }

bindPort :: IO Socket
bindPort = withSocketsDo . listenOn . PortNumber . fromIntegral . read $ "80"

haskellWeb :: Socket -> IO ()
haskellWeb socket = withSocketsDo $ do
    syslog Notice "HaskellWeb running"
    sockHandler socket

sockHandler :: Socket -> IO ()
sockHandler socket = do
    (handle, host, _) <- accept socket
    hSetBuffering handle NoBuffering
    forkIO $ requestHandler handle host
    sockHandler socket

requestHandler :: Handle -> String -> IO ()
requestHandler handle host = do
    request <- hGetContents handle
    --logRequest request host
    requestParser handle request

logRequest :: String -> String -> IO ()
logRequest request host = do
    pT <- getPOSIXTime
    let t = posixSecondsToUTCTime pT
    let log =  show t ++ "\t" ++ host ++ "\t" ++ request ++ "\n"
    appendFile "/var/www/logs/server.log" log

-- Parses HTTP request of the form "GET /foo/bar/baz.html HTTP/1.1\r"
requestParser :: Handle -> String -> IO ()
requestParser handle request = do
    let httpReq = parseHTTP request
    let path = reqURL httpReq
    let site = stripWWW $ reqSite httpReq
    if null (stripFS path) then
        servePage handle site "/index.html"
    else
        servePage handle site path

servePage :: Handle -> String -> String -> IO ()
servePage handle site path = do
    let fullPath = "/var/www/" ++ site ++ path
    let notFoundPath = "/var/www/" ++ site ++ "/404.html"
    pathExists <- fileExist fullPath
    if pathExists
        then do -- hook here?
            serveHeader handle fullPath "200 OK"
            serveFile handle fullPath
        else do
            serveHeader handle notFoundPath "404 Not Found"
            serveFile handle notFoundPath

serveFile :: Handle -> String -> IO ()
serveFile handle path = do
    fileContents <- ByteS.readFile path
    ByteS.hPutStr handle fileContents
    hClose handle -- sClose handle ??

serveHeader :: Handle -> String -> String -> IO ()
serveHeader handle path status = do
    fileSize <- getFileSize path
    let contentType = getContentType path
    hPutStr handle $ "HTTP/1.1 " ++ status ++ "\r\n"
    hPutStr handle $ "Content-Type: " ++ contentType ++ "\r\n"
    hPutStr handle $ "Content-Length: " ++ show fileSize ++ "\r\n"
    hPutStr handle "\r\n"

getContentType :: String -> String
getContentType path 
    | extension == ".html"        = "text/html"
    | extension == ".txt"         = "text/plain"
    | extension == ".css"         = "text/css"
    | extension =~ "(.jpeg|.jpg)" = "image/jpeg"
    | extension == ".png"         = "image/png"
    | extension == ".gif"         = "image/gif"
    | otherwise = "text/plain"
    where extension = takeExtension path

stripWWW :: String -> String
stripWWW ('w':'w':'w':'.':xs) = xs
stripWWW xs = xs

stripFS :: String -> String
stripFS ('/':xs) = xs
stripFS xs = xs

getFileSize :: String -> IO Integer
getFileSize path =
    openFile path ReadMode >>= hFileSize 
