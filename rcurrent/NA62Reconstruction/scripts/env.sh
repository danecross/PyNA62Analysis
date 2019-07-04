#!/bin/sh
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export NA62MCSOURCE=$(dirname $(dirname $DIR))/NA62MC
export NA62RECOSOURCE=$(dirname $DIR)

source ${NA62MCSOURCE}/scripts/env.sh

export PATH=${NA62RECOSOURCE}/bin-${SYSTEMINSTALL}:${PATH}
export LD_LIBRARY_PATH=${NA62RECOSOURCE}/lib-${SYSTEMINSTALL}:${LD_LIBRARY_PATH}

export CMAKE_PREFIX_PATH=${NA62RECOSOURCE}/config:${CMAKE_PREFIX_PATH}

#Uncomment to enable DIM communication
#export DIMDEFINED=1
if [ -z "$ONLINEHLTDEFINED" ]; then
    export ONLINEHLTDEFINED=0
fi

#Conditions DataBase (CDB) variables
if [ -z "$NA62CDBDIR" ]; then
    export NA62CDBDIR=/cvmfs/na62.cern.ch/offline/CDB
fi
