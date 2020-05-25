#!/bin/bash


basename=`basename $0`
deploymentDir=$(dirname $0)


function usage {
   cat <<USAGE
$basename - Control livid server

This script tries to make sure only one instance of the livid server
binary is running at any time, without using a pid or lockfile.

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


binary=lividd
cd $deploymentDir

case "$1" in
   start|restart)
      killall -q $binary
      nohup stack exec $binary 2>&1 >> lividd.log &
      ;;
   stop)
      killall -q $binary
      ;;
   *) usage ;;
esac
