echo "Sourcing $PWD"

defenv=1
if [ -z "$NA62RECOSOURCE" ]; then
	echo Set NA62RECOSOURCE first
	defenv=0
fi

if [ "$defenv" -eq 1 ]; then
        export FWSHELL=sh

        source $NA62RECOSOURCE/scripts/env.sh
        
        if [[ ! "$CMAKE_PREFIX_PATH" =~ "${NA62RECOSOURCE}/config" ]]; then
            export CMAKE_PREFIX_PATH=${NA62RECOSOURCE}/config:${CMAKE_PREFIX_PATH}
        fi

fi

