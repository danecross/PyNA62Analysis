#!/bin/bash

BUILDDIR=$CI_PROJECT_DIR
TESTDIR=$BUILDDIR/test_mc_reco_ana
FWDIR=$BUILDDIR/NA62Analysis

run_and_check(){
  cmd=$1
  echo -e "\e[1;93m$cmd\e[0m"
  $cmd
  retcod=$?
  if [ $retcod -ne 0 ]; then
    exit $retcod
  fi
}

source "$BUILDDIR/NA62Reconstruction/scripts/env.sh"
source "$BUILDDIR/NA62Analysis/scripts/env.sh"

export ANALYSISFW_PATH=$FWDIR
export ANALYSISFW_USERDIR=$FWDIR
export LD_LIBRARY_PATH=$ANALYSISFW_PATH/lib-${SYSTEMINSTALL}:$LD_LIBRARY_PATH

cd "$TESTDIR"

# create dummy pi0Symm1 file for MC
echo "# FitRangeMin FitRangeMax FitPar[0] ... FitPar[7]" > LKr-Pi0CalibrationSymm1.dat
echo "0.0 100000.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0"     >> LKr-Pi0CalibrationSymm1.dat

run_and_check "$FWDIR/bin-${SYSTEMINSTALL}/PostProcessing -i pnn.reco.root -o pnn.postpro.step1.root"

run_and_check "$FWDIR/bin-${SYSTEMINSTALL}/PostProcessing -i pnn.postpro.step1.root -o pnn.postpro.step2.root --histo"
