#!/bin/bash

BUILDDIR=$CI_PROJECT_DIR
TESTDIR=$BUILDDIR/test_mc_reco_ana
DATADIR=/data
FWDIR=$BUILDDIR/NA62Reconstruction

source $FWDIR/scripts/env.sh

run_and_check(){
  cmd=$1
  echo -e "\e[1;93m$cmd\e[0m"
  $cmd
  retcod=$?
  if [ $retcod -ne 0 ]; then
    exit $retcod
  fi
}

if [ ! -d "$TESTDIR" ]; then
  mkdir $TESTDIR
fi
cd $TESTDIR

# Write your test script here

# Every command that should fail the pipeline if failing should be passed to the run_and_check function
