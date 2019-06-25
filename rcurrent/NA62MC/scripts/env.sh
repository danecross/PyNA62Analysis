#!/bin/sh

if [ -z "$NA62TOOLSSOURCE" ]; then
    DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    export NA62TOOLSSOURCE=$(dirname $(dirname $DIR))/NA62Tools
fi
source $NA62TOOLSSOURCE/scripts/env.sh

if [ -z "$NA62MCSOURCE" ]; then
    export NA62MCSOURCE=$(dirname ${NA62TOOLSSOURCE}$)/NA62MC
fi
if [ -z "$G4WORKDIR" ]; then
    export G4WORKDIR=${NA62MCSOURCE}
fi

#flag to enable (1) or disable (0) using the NA62 offline db
export USE_NA62DB=0
if [ "$USE_NA62DB" -gt 0 ]; then
    export NA62DB=${NA62MCSOURCE}/../NA62DB
    source ${NA62DB}/scripts/env.sh
fi

if [ ! -d /cvmfs/sft.cern.ch ];then
    echo "[NA62FW] WARNING: AFS is not supported any longer!"
    return
fi

#export OGLHOME=/usr/X11R6
#export OGLLIBS="-L${OGLHOME}/lib64 -lGLU -lGL"

# Add NA62MC-related bin/lib dirs to the env PATHs
export PATH=${NA62MCSOURCE}/bin-${SYSTEMINSTALL}:${PATH}
export LD_LIBRARY_PATH=${NA62MCSOURCE}/lib-${SYSTEMINSTALL}/:$LD_LIBRARY_PATH
export CMAKE_PREFIX_PATH=${NA62MCSOURCE}/config:${CMAKE_PREFIX_PATH}

## pointing to patched G4 lib for mu polarisation
## NB: THIS PATCH SHOULD STAY BEFORE THE G4LIBS IN THE LD_LIBRARY_PATH!
if [ "$SYSTEMINSTALL" = "slc6" ]; then
  ## force a local LD_LIBRARY_PATH search
  ## as compiled location not available on CVMFS
  export LD_LIBRARY_PATH=${G4WORKDIR}/tmp/${G4SYSTEM}/NA62MC:$LD_LIBRARY_PATH
  export PATH=${G4WORKDIR}/bin/${G4SYSTEM}:${NA62MCSOURCE}/bin-${SYSTEMINSTALL}:${PATH}
  export LD_LIBRARY_PATH=/cvmfs/na62.cern.ch/offline/NA62FW/G410.02.p02-patch:$LD_LIBRARY_PATH
fi
