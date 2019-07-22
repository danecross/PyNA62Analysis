echo "Sourcing $PWD"

## FW top directory
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DIR=$(dirname $(dirname ${DIR}))

if [ ! -z "$NA62TOOLSSOURCE" ]; then
    source ${NA62TOOLSSOURCE}/scripts/functions.sh
fi

# Source NA62Reconstruction if not yet done
if [ -z "$NA62RECOSOURCE" ]; then
    source ${DIR}/NA62Reconstruction/scripts/env.sh
fi

export FWSHELL=sh

pathappend CMAKE_PREFIX_PATH ${NA62RECOSOURCE}/config

