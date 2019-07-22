#!/bin/bash
#
# Usage: ./scripts/run.sh [MacroFile] [RunNumber]
# Defaults: MacroFile =  macros/StandardRun.mac, RunNumber = 8134
#

uname -a
source scripts/env.sh

macfile="macros/StandardRun.mac"
if [ ! -z $1 ]; then
    macfile=$1
fi

if [ ! -f $macfile ]; then
    echo Error: macro file $macfile does not exist
    exit
fi

run=8134
if [ ! -z $2 ]; then
    run=$2
fi

echo Executing NA62MC $macfile $run
NA62MC $macfile $run
