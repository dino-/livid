#!/bin/bash


basename=`basename $0`


function usage {
   cat <<USAGE
$basename - Control vidproj server

usage:
   $basename [OPTIONS] COMMAND

options:
   -h, --help  This help info

commands:
   start     Laptop display only
   stop      External monitor only
   restart   Both of the above outputs, laptop on the left

USAGE

   exit 1
}


binFullPath="dist/build/vidproj-srv/vidproj-srv"
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
