#!/bin/bash


basename=`basename $0`


function usage {
   cat <<USAGE
$basename - Control livid server

usage:
   $basename [OPTIONS] COMMAND

options:
   -h, --help  This help info

commands:
   start     Start the server
   stop      Stop the server
   restart   Stop and then start the server again

USAGE

   exit 1
}


binFullPath="dist/build/livid-srv/livid-srv"
binary=`basename $binFullPath`

case "$1" in
   start|restart)
      killall $binary
      $binFullPath &
      ;;
   stop)
      killall $binary
      ;;
   *) usage ;;
esac
