HaskellWeb
==========

Simple static HTTP web server in Haskell. Only responds to requests of the form
"GET /foo/bar/baz.html HTTP/1.1\r"

Logs in /var/www/logs/server.log as TIME/DATE HOST REQUEST

Fully daemonized - control with {start, stop, restart}

TODO:
	Configuration file - set log settings and file paths
	Respond to more HTTP requests (HEAD, etc)
	Syntax
	Server statistics