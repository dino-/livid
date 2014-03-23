# livid


## Synopsis

LIst of VIDeos media browse/playback web app (Haskell, Coffeescript)


## Description

livid is a web application for browsing a directory tree of
video files, launching playback and later deleting those files if
desired. It's designed to be run locally on a system from within a
gui environment so it can launch a video player application (like
vlc, for example).

This application uses a vaguely-RESTful interface between the client
and server. The client is a single-page design, with data encoded
in JSON.


## Getting source

Get the source with darcs:

>     $ darcs get http://ui3.info/darcs/livid

Or [browse the source](http://ui3.info/darcs/livid)


## Installing

The server is written in Haskell. To build it:

>     $ cabal configure
>     $ cabal build

The client is written in Coffeescript. To build it:

>     $ cake build

Upon successful compilation, you can start the server:

>     $ ./livid-srv.sh start|stop|restart

And then load the client in your browser:

>     http://localhost:8082

In the client UI, navigate with the mouse or arrow keys, press
`Enter` to play a video and `Del` or `d` to delete a file.

Various configuration options can be adjusted in the file
`livid.conf` such as HTTP port, directories to scan for files as
well as file types and video playback command.


## Contact

### Reporting Bugs

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>

### Authors

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>  
Betty Diegel <[bdiegel@usa.net](mailto:bdiegel@usa.net)>
