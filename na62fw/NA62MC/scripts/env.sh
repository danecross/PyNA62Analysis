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

export NA62MCSOURCE=${DIR}/NA62MC

if [ -z "$G4WORKDIR" ]; then
    export G4WORKDIR=${NA62MCSOURCE}
fi

#flag to enable (1) or disable (0) using the NA62 offline db
export USE_NA62DB=0
if [ "$USE_NA62DB" -gt 0 ]; then
    export NA62DB=${DIR}/NA62DB
    source ${NA62DB}/scripts/env.sh
fi

if [ ! -d /cvmfs/sft.cern.ch ];then
    echo "[NA62FW] WARNING: AFS is not supported any longer!"
    return
fi

#export OGLHOME=/usr/X11R6
#export OGLLIBS="-L${OGLHOME}/lib64 -lGLU -lGL"

# Add NA62MC-related bin/lib dirs to the env PATHs
pathappend PATH ${NA62MCSOURCE}/bin-${SYSTEMINSTALL}
pathappend LD_LIBRARY_PATH ${NA62MCSOURCE}/lib-${SYSTEMINSTALL}/
pathappend CMAKE_PREFIX_PATH ${NA62MCSOURCE}/config

## pointing to patched G4 lib for mu polarisation
## NB: THIS PATCH SHOULD STAY BEFORE THE G4LIBS IN THE LD_LIBRARY_PATH!
if [ "$SYSTEMINSTALL" = "slc6" ]; then
  ## force a local LD_LIBRARY_PATH search
  ## as compiled location not available on CVMFS
  pathappend PATH ${G4WORKDIR}/bin/${G4SYSTEM} ${NA62MCSOURCE}/bin-${SYSTEMINSTALL}
  pathappend LD_LIBRARY_PATH /cvmfs/na62.cern.ch/offline/NA62FW/G410.02.p02-patch
fi
