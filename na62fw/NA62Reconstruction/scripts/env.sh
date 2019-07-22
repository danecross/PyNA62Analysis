#!/bin/sh

## FW top directory
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DIR=$(dirname $(dirname ${DIR}))

# Source NA62Tools if not yet done
if [ -z "$NA62TOOLSSOURCE" ]; then
    source ${DIR}/NA62Tools/scripts/env.sh
else
    # Make sure pathappend exists
    source ${NA62TOOLSSOURCE}/scripts/functions.sh
fi

export NA62RECOSOURCE=${DIR}/NA62Reconstruction

pathappend PATH ${NA62RECOSOURCE}/bin-${SYSTEMINSTALL}
pathappend LD_LIBRARY_PATH ${NA62RECOSOURCE}/lib-${SYSTEMINSTALL}

pathappend CMAKE_PREFIX_PATH ${NA62RECOSOURCE}/config

#Uncomment to enable DIM communication
#export DIMDEFINED=1
if [ -z "$ONLINEHLTDEFINED" ]; then
    export ONLINEHLTDEFINED=0
fi

#Conditions DataBase (CDB) variables
if [ -z "$NA62CDBDIR" ]; then
    export NA62CDBDIR=/cvmfs/na62.cern.ch/offline/CDB
fi
