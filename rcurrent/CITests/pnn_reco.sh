#!/bin/bash

BUILDDIR=$CI_PROJECT_DIR
TESTDIR=$BUILDDIR/test_mc_reco_ana
FWDIR=$BUILDDIR/NA62Reconstruction

run_and_check(){
  cmd=$1
  echo -e "\e[1;93m$cmd\e[0m"
  $cmd
  retcod=$?
  if [ $retcod -ne 0 ]; then
    exit $retcod
  fi
}

source "$FWDIR/scripts/env.sh"

cp -r "$FWDIR/config" "$TESTDIR"

cd "$TESTDIR"
run_and_check "$FWDIR/bin-${SYSTEMINSTALL}/NA62Reco -i pnn.mc.root -o pnn.reco.root -e1 -c config/NA62Reconstruction.MC.conf"
