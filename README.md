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

    $ git clone https://github.com/dino-/livid.git

Or [browse the source](https://github.com/dino-/livid)


## Building and starting the server

### The coffeescript/web parts

Install the coffeescript tools locally. WARNING: The @ version here is very
important. The generated JavaScript will not work with a modern Coffeescript
like 2.5.1 or later!!

    $ npm install --save-dev coffeescript@1.12.4

And then fix up the environment in this shell

    $ . util/coffee-env

To build the web client:

    $ cake build

For more info, read about CoffeeScript's 
[cake build utility](https://coffeescript.org/#installation)

### The Haskell lividd server part

The server is written in Haskell. To build it:

    $ stack build

For more info, read about the 
[Haskell stack build utility](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

### Building the distributable package

Deploy everything into the the package directory. Note: You will need hsinstall
v2.6 or later.

    $ hsinstall

Perform the Debian packaging

    $ ./util/package.sh

Once this is installed, you need to copy two files into the home directory of the user who is running X

    $ cd
    $ cp /usr/share/livid/livid.conf .config/
    $ mkdir -p .config/systemd/user
    $ cp /usr/share/livid/systemd/user/lividd.service .config/systemd/user

Edit the livid.conf file to reflect your video location, etc.

Start the systemd service:

    $ systemctl --user daemon-reload
    $ systemctl --user start lividd

Don't use systemctl enable on this service to start it after X starts. Instead,
use your desktop environment's method of setting startup programs. Put the
above systemctl start command in there.

You should see good log output

    $ journalctl --user -u lividd -f

### Using the app

And then load the client in your browser:

    http://localhost:8082

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
