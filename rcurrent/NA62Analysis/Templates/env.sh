export ANALYSISFW_PATH=$$ANALYSISFW$$
export ANALYSISFW_USERDIR=$$USERDIR$$
export NA62RECOSOURCE=$$NA62RECOSOURCE$$

source $ANALYSISFW_PATH/scripts/env.sh

if [[ ! $LD_LIBRARY_PATH =~ $ANALYSISFW_PATH/lib-${SYSTEMINSTALL} ]]; then
	export LD_LIBRARY_PATH=$ANALYSISFW_PATH/lib-${SYSTEMINSTALL}:$LD_LIBRARY_PATH
	export LD_LIBRARY_PATH=$ANALYSISFW_USERDIR/lib-${SYSTEMINSTALL}:$LD_LIBRARY_PATH
	export PATH=$ANALYSISFW_PATH:$ANALYSISFW_PATH/bin-${SYSTEMINSTALL}:$ANALYSISFW_USERDIR/bin-${SYSTEMINSTALL}:$PATH
fi
