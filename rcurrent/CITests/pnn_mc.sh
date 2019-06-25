#!/bin/bash

BUILDDIR=$CI_PROJECT_DIR
TESTDIR=$BUILDDIR/test_mc_reco_ana
FWDIR=$BUILDDIR/NA62MC

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

mkdir "$TESTDIR"

cd "$TESTDIR"
run_and_check "$FWDIR/bin-${SYSTEMINSTALL}/NA62MC $FWDIR/macros/StandardRun.mac"

mv pluto.root pnn.mc.root
