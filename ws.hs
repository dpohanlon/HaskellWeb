import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import GHC.IO.Handle (hClose)
import System.Environment (getArgs)
import System.FilePath.Posix (takeExtension)
import System.Posix.Files (fileExist)
import System.IO (hSetBuffering, hGetLine, hPutStr, hPutStrLn, hFileSize, openFile, IOMode(ReadMode), FilePath, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import Text.Regex.Posix
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import System.Posix.Daemonize
import qualified Data.ByteString as ByteS

main :: IO ()
main = serviced listen

listen :: IO ()
listen = withSocketsDo $ do
	socket <- listenOn $ PortNumber (fromIntegral (read "80"))
	sockHandler socket

sockHandler :: Socket -> IO ()
sockHandler socket = do
	(handle, host, _) <- accept socket
	hSetBuffering handle NoBuffering
	forkIO $ requestHandler handle host
	sockHandler socket

requestHandler :: Handle -> String -> IO ()
requestHandler handle host = do
	request <- hGetLine handle
	logRequest request host
	requestParser handle request

logRequest :: String -> String -> IO ()
logRequest request host = do
	pT <- getPOSIXTime
	let t = posixSecondsToUTCTime pT
	let log =  show t ++ "\t" ++ host ++ "\t" ++ request ++ "\n"
	appendFile "/var/www/logs/server.log" log


requestParser :: Handle -> String -> IO ()
requestParser handle request = do
	let path =  drop 2 (request =~ " /[a-zA-Z0-9./-_]{0,}" :: String) -- Parses input of the form "GET /foo/bar/baz.html HTTP/1.1\r"
	if null path then
		servePage handle (path ++ "index.html")
	else
		servePage handle path

serveNotFound :: Handle -> IO ()
serveNotFound handle = do
	serveHeader handle "404.html" "404 Not Found"
	serveFile handle "404.html"

servePage :: Handle -> String -> IO ()
servePage handle path = do
	exists <- fileExist path
	if exists
		then do
			serveHeader handle path "200 OK"
			serveFile handle path
		else
			serveNotFound handle


serveFile :: Handle -> String -> IO ()
serveFile handle path = do
	fileContents <- ByteS.readFile path
	ByteS.hPutStr handle fileContents
	hClose handle -- sClose handle ??

serveHeader :: Handle -> String -> String -> IO ()
serveHeader handle path status = do
	fileSize <- (getFileSize path)
	let contentType = getContentType path
	hPutStr handle $ "HTTP/1.1 " ++ status ++ "\r\n"
	hPutStr handle $ "Content-Type: " ++ contentType ++ "\r\n"
	hPutStr handle $ "Content-Length: " ++ show fileSize ++ "\r\n"
	hPutStr handle "\r\n"

getContentType :: String -> String
getContentType path 
	| extension == ".html" = "text/html"
	| extension == ".txt"  = "text/plain"
	| extension == ".css"  = "text/css"
	| extension =~ "(.jpeg|.jpg)" = "image/jpeg"
	| extension == ".png"  = "image/png"
	| extension == ".gif"  = "image/gif"
	| otherwise = "text/plain"
	where extension = takeExtension path

getFileSize :: String -> IO Integer
getFileSize path = do
	h <- openFile path ReadMode 
	hFileSize h
